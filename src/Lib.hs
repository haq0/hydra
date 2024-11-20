{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Lib 
    ( countLines
    , prettyPrint
    , Config(..)
    , FileStats(..)
    , Language(..)
    , hideProgressBar
    , verboseLog
    , verboseLogF
    ) where

import Data.IORef
import System.Console.ANSI (SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..), setSGR, clearLine, cursorUpLine, setSGRCode, ConsoleIntensity( BoldIntensity ))
import Control.Concurrent (threadDelay)
import qualified Data.Text.Lazy as TL
import Control.Monad (void, forM, MonadPlus, mzero, guard, when, foldM)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.Directory
import System.FilePath
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Text.Printf (printf)
import Control.Monad.Trans.State.Strict (StateT)
import Debug.Trace (trace)
import System.ProgressBar
import qualified System.ProgressBar as PB
import System.IO (hFlush, stdout)
import Data.Text (pack)

type Parser = Parsec Void Text

data Config = Config { configPath :: FilePath, configVerbose :: Bool } deriving (Show)
data ProcessingState = ProcessingState
    { psCurrentFile :: Int
    , psTotalFiles :: Int
    } deriving (Show)

data Language = 
    Haskell
  | Python
  | C
  | CPP
  | JavaScript
  | Java
  | Ruby
  | Rust
  | Go
  | Swift
  | Kotlin
  | PHP
  | Shell
  | HTML
  | CSS
  | SQL
  deriving (Show, Eq, Ord)

languageColor :: Language -> Color
languageColor = \case
    Haskell -> Magenta
    Python -> Blue
    C -> Red
    CPP -> Red
    JavaScript -> Yellow
    Java -> Red
    Ruby -> Red
    Rust -> Yellow
    Go -> Cyan
    Swift -> Green
    Kotlin -> Green
    PHP -> Magenta
    Shell -> Green
    HTML -> Blue
    CSS -> Blue
    SQL -> Cyan

data LineType = 
    BlankLine
  | CodeLine Language
  | CommentLine CommentType
  deriving (Show, Eq)

data CommentType = 
    SingleLine
  | MultiLine
  deriving (Show, Eq)

data FileStats = FileStats
    { fsBlankLines :: !Int
    , fsCodeLines :: !Int
    , fsSingleComments :: !Int
    , fsMultiComments :: !Int
    , fsFileCount :: !Int
    } deriving (Show, Eq)

emptyStats :: FileStats
emptyStats = FileStats 0 0 0 0 1

data LangConfig = LangConfig
    { lcExtensions :: [String]
    , lcSingleLineComment :: [Text]
    , lcMultiLineStart :: Text
    , lcMultiLineEnd :: Text
    , lcNestedComments :: Bool
    }


verboseLog :: Bool -> String -> IO ()
verboseLog verbose msg = when verbose $ do
    putStrLn $ "\x1b[36m[DEBUG]\x1b[0m " ++ msg
    hFlush stdout

verboseLogF :: Bool -> String -> String -> IO ()
verboseLogF verbose prefix msg = when verbose $ do
    putStrLn $ "\x1b[36m[DEBUG:" ++ prefix ++ "]\x1b[0m " ++ msg
    hFlush stdout


countSourceFiles :: FilePath -> IO Int
countSourceFiles path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> case detectLanguage path of
                        Just _ -> return 1
                        Nothing -> return 0
        (_, True) -> do
            contents <- listDirectory path
            counts <- forM contents $ \name -> 
                countSourceFiles (path </> name)
            return $ sum counts
        _ -> return 0

languageConfigs :: Map Language LangConfig
languageConfigs = Map.fromList
    [ (Haskell, LangConfig 
        { lcExtensions = [".hs", ".lhs"]
        , lcSingleLineComment = ["--"]
        , lcMultiLineStart = "{-"
        , lcMultiLineEnd = "-}"
        , lcNestedComments = True
        })
    , (Python, LangConfig
        { lcExtensions = [".py"]
        , lcSingleLineComment = ["#"]
        , lcMultiLineStart = "\"\"\""
        , lcMultiLineEnd = "\"\"\""
        , lcNestedComments = False
        })
    , (C, LangConfig
        { lcExtensions = [".c", ".h"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (CPP, LangConfig
        { lcExtensions = [".cpp", ".hpp", ".cc", ".hxx"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (JavaScript, LangConfig
        { lcExtensions = [".js", ".jsx"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (Java, LangConfig
        { lcExtensions = [".java"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (Ruby, LangConfig
        { lcExtensions = [".rb"]
        , lcSingleLineComment = ["#"]
        , lcMultiLineStart = "=begin"
        , lcMultiLineEnd = "=end"
        , lcNestedComments = False
        })
    , (Rust, LangConfig
        { lcExtensions = [".rs"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = True
        })
    , (Go, LangConfig
        { lcExtensions = [".go"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (Swift, LangConfig
        { lcExtensions = [".swift"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (Kotlin, LangConfig
        { lcExtensions = [".kt", ".kts"]
        , lcSingleLineComment = ["//"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (PHP, LangConfig
        { lcExtensions = [".php"]
        , lcSingleLineComment = ["//", "#"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (Shell, LangConfig
        { lcExtensions = [".sh", ".bash", ".zsh"]
        , lcSingleLineComment = ["#"]
        , lcMultiLineStart = ""
        , lcMultiLineEnd = ""
        , lcNestedComments = False
        })
    , (HTML, LangConfig
        { lcExtensions = [".html", ".htm"]
        , lcSingleLineComment = []
        , lcMultiLineStart = "<!--"
        , lcMultiLineEnd = "-->"
        , lcNestedComments = False
        })
    , (CSS, LangConfig
        { lcExtensions = [".css"]
        , lcSingleLineComment = []
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    , (SQL, LangConfig
        { lcExtensions = [".sql"]
        , lcSingleLineComment = ["--"]
        , lcMultiLineStart = "/*"
        , lcMultiLineEnd = "*/"
        , lcNestedComments = False
        })
    ]

data ParserState = ParserState
    { psLanguage :: Language
    , psInComment :: Bool
    , psCommentDepth :: Int
    } deriving Show

type StatefulParser = ParsecT Void Text (StateT ParserState IO)

initialState :: Language -> ParserState
initialState lang = ParserState lang False 0

spaceConsumer :: (MonadParsec e Text m) => m ()
spaceConsumer = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: (MonadParsec e Text m) => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: (MonadParsec e Text m) => Text -> m Text
symbol = L.symbol spaceConsumer

eol :: (MonadParsec e Text m) => m ()
eol = void $ chunk "\n" <|> chunk "\r\n"

eol' :: (MonadParsec e Text m) => m ()
eol' = void newline <|> eof

restOfLine :: (MonadParsec e Text m) => m Text
restOfLine = takeWhileP Nothing (/= '\n')

multiLineComment :: (MonadParsec e Text m, MonadState ParserState m, MonadPlus m) => m LineType
multiLineComment = do
    state <- get
    let config = languageConfigs Map.! psLanguage state
    guard (not $ T.null (lcMultiLineStart config) && T.null (lcMultiLineEnd config))  
    try $ do
        void $ chunk (lcMultiLineStart config)
        modify (\s -> s { psInComment = True, psCommentDepth = psCommentDepth s + 1 })
        content <- manyTill anySingle (try $ do
            if lcNestedComments config
                then checkNesting config
                else void $ chunk (lcMultiLineEnd config))
        modify (\s -> s { psInComment = psCommentDepth s > 1, psCommentDepth = psCommentDepth s - 1 })
        return $ CommentLine MultiLine
  where
    checkNesting :: (MonadParsec e Text m, MonadState ParserState m, MonadPlus m) => LangConfig -> m ()
    checkNesting config = do
        state <- get
        if psCommentDepth state == 1
            then void $ chunk (lcMultiLineEnd config)
            else (void $ chunk (lcMultiLineEnd config)) <|> 
                 (void (chunk (lcMultiLineStart config)) >> 
                  modify (\s -> s { psCommentDepth = psCommentDepth s + 1 }) >>
                  mzero)

singleLineComment :: (MonadParsec e Text m, MonadState ParserState m) => m LineType
singleLineComment = do
    state <- get
    let config = languageConfigs Map.! psLanguage state
    try $ do
        spaceConsumer
        choice [try (void $ chunk marker) >> restOfLine >> eol' >> return (CommentLine SingleLine) 
               | marker <- lcSingleLineComment config]

codeLine :: (MonadParsec e Text m, MonadState ParserState m) => m LineType
codeLine = do
    state <- get
    spaceConsumer
    let config = languageConfigs Map.! psLanguage state
    notFollowedBy (choice [chunk marker | marker <- lcSingleLineComment config] <|> 
                  chunk (lcMultiLineStart config))
    content <- restOfLine
    void newline <|> eof  
    return $ if T.null (T.strip content)
             then BlankLine
             else CodeLine (psLanguage state)

blankLine :: (MonadParsec e Text m) => m LineType
blankLine = do
    try $ do
        spaceConsumer
        lookAhead (void newline <|> eof)  
        void newline <|> eof
        return BlankLine

lineParser :: (MonadParsec e Text m, MonadState ParserState m, MonadPlus m) => m LineType
lineParser = do
    notFollowedBy eof  
    result <- try blankLine 
          <|> try multiLineComment
          <|> try singleLineComment
          <|> codeLine
    return result

fileParser :: StatefulParser [LineType]
fileParser = do
    lines <- manyTill lineParser eof  
    return lines

processFile :: FilePath -> Bool -> IO FileStats
processFile path verbose = do
    verboseLogF verbose "FILE" $ "Reading file: " ++ path
    content <- TIO.readFile path
    case detectLanguage path of
        Nothing -> do
            verboseLogF verbose "FILE" $ "No language detected for: " ++ path
            return emptyStats
        Just language -> do
            verboseLogF verbose "PARSE" $ "Parsing " ++ show language ++ " file: " ++ path
            let initialSt = initialState language
            result <- runParserT fileParser path content `evalStateT` initialSt
            case result of
                Left err -> do
                    verboseLogF verbose "ERROR" $ "Parse error in " ++ path ++ ": " ++ show err
                    return emptyStats
                Right lineTypes -> do
                    let stats = foldl updateStats emptyStats lineTypes
                    verboseLogF verbose "STATS" $ "File statistics for " ++ path ++ ": " ++ show stats
                    return stats
updateStats :: FileStats -> LineType -> FileStats
updateStats stats BlankLine = stats { fsBlankLines = fsBlankLines stats + 1 }
updateStats stats (CodeLine _) = stats { fsCodeLines = fsCodeLines stats + 1 }
updateStats stats (CommentLine SingleLine) = stats { fsSingleComments = fsSingleComments stats + 1 }
updateStats stats (CommentLine MultiLine) = stats { fsMultiComments = fsMultiComments stats + 1 }

detectLanguage :: FilePath -> Maybe Language
detectLanguage path = 
    let ext = takeExtension path
    in Map.foldlWithKey' (\acc lang config -> 
        if ext `elem` lcExtensions config 
            then Just lang 
            else acc) Nothing languageConfigs

processDirectory :: ProgressBar () -> FilePath -> Bool -> IO (Map Language FileStats)
processDirectory pb path verbose = do
    verboseLogF verbose "DIR" $ "Scanning directory: " ++ path
    contents <- listDirectory path
    verboseLogF verbose "DIR" $ "Found " ++ show (length contents) ++ " entries in " ++ path
    foldM (processEntry pb verbose) Map.empty contents
  where
    processEntry :: ProgressBar () -> Bool -> Map Language FileStats -> FilePath -> IO (Map Language FileStats)
    processEntry pb verbose acc name = do
        let fullPath = path </> name
        verboseLogF verbose "ENTRY" $ "Processing entry: " ++ fullPath
        isFile <- doesFileExist fullPath
        isDir <- doesDirectoryExist fullPath
        case (isFile, isDir) of
            (True, _) -> do
                case detectLanguage fullPath of
                    Nothing -> do
                        verboseLogF verbose "SKIP" $ "Skipping non-source file: " ++ fullPath
                        return acc
                    Just lang -> do
                        verboseLogF verbose "PROCESS" $ "Processing " ++ show lang ++ " file: " ++ fullPath
                        stats <- processFile fullPath verbose
                        updateProgress pb $ \p -> p { progressDone = progressDone p + 1 }
                        return $ Map.insertWith combineStats lang stats acc
            (_, True) -> do
                verboseLogF verbose "DIR" $ "Entering directory: " ++ fullPath
                subStats <- processDirectory pb fullPath verbose
                verboseLogF verbose "DIR" $ "Finished directory: " ++ fullPath
                return $ Map.unionWith combineStats acc subStats
            _ -> do
                verboseLogF verbose "SKIP" $ "Skipping invalid path: " ++ fullPath
                return acc
combineStats :: FileStats -> FileStats -> FileStats
combineStats (FileStats b1 c1 s1 m1 f1) (FileStats b2 c2 s2 m2 f2) =
    FileStats (b1 + b2) (c1 + c2) (s1 + s2) (m1 + m2) (f1 + f2)

countLines :: FilePath -> Bool -> IO (Map Language FileStats)
countLines path verbose = do
    verboseLog verbose "Starting line counting process..."
    totalFiles <- countSourceFiles path
    verboseLogF verbose "INIT" $ "Found " ++ show totalFiles ++ " source files to process"

    let style = defStyle
          { styleWidth = TerminalWidth 60
          , stylePrefix = percentage
          , stylePostfix = exact <> " " <> elapsedTime renderDuration
          , styleDone = '='
          , styleCurrent = '>'
          , styleTodo = ' '
          }

    verboseLog verbose "Creating progress bar..."
    pb <- newProgressBar style 30 (Progress 0 totalFiles ())

    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    verboseLogF verbose "PATH" $ "Path type: " ++ if isFile then "File" else if isDir then "Directory" else "Unknown"

    result <- case (isFile, isDir) of
        (True, _) -> do
            verboseLogF verbose "FILE" $ "Processing single file: " ++ path
            stats <- processFile path verbose
            updateProgress pb $ \p -> p { progressDone = progressDone p + 1 }
            case detectLanguage path of
                Nothing -> do
                    verboseLogF verbose "FILE" "Could not detect language"
                    return Map.empty
                Just lang -> do
                    verboseLogF verbose "FILE" $ "Detected language: " ++ show lang
                    return $ Map.singleton lang stats
        (_, True) -> do
            verboseLogF verbose "DIR" $ "Processing directory: " ++ path
            processDirectory pb path verbose
        _ -> do
            verboseLogF verbose "ERROR" $ "Path does not exist: " ++ path
            return Map.empty

    verboseLog verbose "Processing complete!"
    hideProgressBar
    putStrLn ""
    return result
processPathWithProgress :: ProgressBar () -> FilePath -> IORef (Map Language FileStats) -> Bool -> IO ()
processPathWithProgress pb path resultsRef verbose = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    case (isFile, isDir) of
        (True, _) -> do
            stats <- processFile path verbose
            case detectLanguage path of
                Nothing -> return ()
                Just lang -> modifyIORef' resultsRef $ 
                    Map.insertWith combineStats lang stats
            updateProgress pb $ \p -> p { progressDone = progressDone p + 1 }

        (_, True) -> do
            contents <- listDirectory path
            for_ contents $ \name -> do
                let fullPath = path </> name
                processPathWithProgress pb fullPath resultsRef verbose

        _ -> return ()
hideProgressBar :: IO ()
hideProgressBar = do
    cursorUpLine 1  
    clearLine      

prettyPrint :: Map Language FileStats -> IO ()
prettyPrint stats = do
    putStrLn "\nSummary:"
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
    putStrLn "Language      Files    Blank   Comment     Code"
    putStrLn "-----------------------------------------------"
    mapM_ printLangStats (Map.toList stats)

    let totalStats = foldl combineStats emptyStats (Map.elems stats)
    printTotalStats totalStats

    setSGR [Reset]
  where
    printLangStats :: (Language, FileStats) -> IO ()
    printLangStats (lang, FileStats{..}) = do
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid (languageColor lang)]
        let langStr = take 12 $ show lang ++ replicate 12 ' '
        printf "%-12s %7d %8d %9d %8d\n" 
            langStr
            (fromIntegral fsFileCount :: Int)
            (fromIntegral fsBlankLines :: Int)
            (fromIntegral (fsSingleComments + fsMultiComments) :: Int)
            (fromIntegral fsCodeLines :: Int)

    printTotalStats :: FileStats -> IO ()
    printTotalStats FileStats{..} = do
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
        putStrLn "-----------------------------------------------"
        printf "%-12s %7d %8d %9d %8d\n" 
            ("Total" :: String)
            (fromIntegral fsFileCount :: Int)
            (fromIntegral fsBlankLines :: Int)
            (fromIntegral (fsSingleComments + fsMultiComments) :: Int)
            (fromIntegral fsCodeLines :: Int)

