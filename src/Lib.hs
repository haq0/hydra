{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Monad       (foldM, forM, unless, when)
import qualified Data.ByteString     as BS
import           Data.List           (foldl')
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import           System.Console.ANSI (Color (..), ColorIntensity (..),
                                      ConsoleIntensity (BoldIntensity),
                                      ConsoleLayer (..), SGR (..), clearLine,
                                      cursorUpLine, setSGR)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      listDirectory)
import           System.FilePath     (takeExtension, (</>))
import           System.IO           (hFlush, stdout)
import           System.ProgressBar  (Progress (Progress, progressDone),
                                      ProgressBar,
                                      ProgressBarWidth (TerminalWidth),
                                      Style (styleCurrent, styleDone, stylePostfix, stylePrefix, styleTodo, styleWidth),
                                      defStyle, elapsedTime, exact,
                                      newProgressBar, percentage,
                                      renderDuration, updateProgress)
import           Text.Megaparsec     ((<|>))
import           Text.Printf         (printf)

data Config = Config
  { configPath    :: FilePath
  , configVerbose :: Bool
  } deriving (Show)

data ProcessingState = ProcessingState
  { psCurrentFile :: Int
  , psTotalFiles  :: Int
  } deriving (Show)

data Language
  = Ada
  | Assembly
  | AWK
  | Bash
  | Basic
  | C
  | COBOL
  | CPP
  | CSharp
  | CSS
  | D
  | Dart
  | Elixir
  | Elm
  | Erlang
  | FSharp
  | Fortran
  | Go
  | Groovy
  | Haskell
  | HTML
  | Java
  | JavaScript
  | JSON
  | Julia
  | Kotlin
  | Lisp
  | Lua
  | MATLAB
  | Nim
  | ObjectiveC
  | OCaml
  | Pascal
  | Perl
  | PHP
  | PowerShell
  | Prolog
  | Python
  | R
  | Racket
  | Ruby
  | Rust
  | Scala
  | Scheme
  | Shell
  | Smalltalk
  | SQL
  | Swift
  | Tcl
  | TypeScript
  | VB
  | VHDL
  | XML
  | Zig
  | Other String
  deriving (Show, Eq, Ord)

detectOtherLanguage :: FilePath -> Maybe Language
detectOtherLanguage path =
  let ext = takeExtension path
   in if ext /= ""
        then Just (Other (drop 1 ext))
        else Nothing

detectLanguage :: FilePath -> Maybe Language
detectLanguage path =
  let ext = takeExtension path
      lookupExt =
        Map.foldlWithKey'
          (\acc lang config ->
             if ext `elem` lcExtensions config
               then Just lang
               else acc)
          Nothing
          languageConfigs
   in lookupExt <|> detectOtherLanguage path

languageColor :: Language -> Color
languageColor =
  \case
    Ada -> Blue
    Assembly -> Red
    AWK -> Green
    Bash -> Green
    Basic -> Blue
    C -> Red
    COBOL -> Blue
    CPP -> Red
    CSharp -> Magenta
    CSS -> Blue
    D -> Red
    Dart -> Blue
    Elixir -> Magenta
    Elm -> Blue
    Erlang -> Red
    FSharp -> Blue
    Fortran -> Green
    Go -> Cyan
    Groovy -> Green
    Haskell -> Magenta
    HTML -> Blue
    Java -> Red
    JavaScript -> Yellow
    JSON -> Yellow
    Julia -> Magenta
    Kotlin -> Green
    Lisp -> Blue
    Lua -> Blue
    MATLAB -> Yellow
    Nim -> Yellow
    ObjectiveC -> Blue
    OCaml -> Yellow
    Pascal -> Green
    Perl -> Blue
    PHP -> Magenta
    PowerShell -> Blue
    Prolog -> Red
    Python -> Blue
    R -> Blue
    Racket -> Red
    Ruby -> Red
    Rust -> Yellow
    Scala -> Red
    Scheme -> Blue
    Shell -> Green
    Smalltalk -> Blue
    SQL -> Cyan
    Swift -> Green
    Tcl -> Yellow
    TypeScript -> Blue
    VB -> Blue
    VHDL -> Green
    XML -> Blue
    Zig -> Yellow
    Other _ -> White

data LineType
  = BlankLine
  | CodeLine Language
  | CommentLine CommentType
  deriving (Show, Eq)

data CommentType
  = SingleLine
  | MultiLine
  deriving (Show, Eq)

data FileStats = FileStats
  { fsBlankLines     :: {-# UNPACK #-} !Int
  , fsCodeLines      :: {-# UNPACK #-} !Int
  , fsSingleComments :: {-# UNPACK #-} !Int
  , fsMultiComments  :: {-# UNPACK #-} !Int
  , fsFileCount      :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

emptyStats :: FileStats
emptyStats = FileStats 0 0 0 0 1

data LangConfig = LangConfig
  { lcExtensions        :: [String]
  , lcSingleLineComment :: [Text]
  , lcMultiLineStart    :: Text
  , lcMultiLineEnd      :: Text
  , lcNestedComments    :: Bool
  }

verboseLog :: Bool -> String -> IO ()
verboseLog verbose msga =
  when verbose $ do
    putStrLn $ "\x1b[36m[DEBUG]\x1b[0m " ++ msga
    hFlush stdout

verboseLogF :: Bool -> String -> String -> IO ()
verboseLogF verbose prefix msga =
  when verbose $ do
    putStrLn $ "\x1b[36m[DEBUG:" ++ prefix ++ "]\x1b[0m " ++ msga
    hFlush stdout

countSourceFiles :: FilePath -> IO Int
countSourceFiles path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, _) ->
      case detectLanguage path of
        Just _  -> return 1
        Nothing -> return 0
    (_, True) -> do
      contents <- listDirectory path
      counts <- forM contents $ \name -> countSourceFiles (path </> name)
      return $ sum counts
    _ -> return 0

languageConfigs :: Map Language LangConfig
languageConfigs =
  Map.fromList
    [ ( Ada
      , LangConfig
          { lcExtensions = [".ada", ".adb", ".ads"]
          , lcSingleLineComment = ["--"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( Assembly
      , LangConfig
          { lcExtensions = [".asm", ".s", ".S"]
          , lcSingleLineComment = [";", "#"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( AWK
      , LangConfig
          { lcExtensions = [".awk"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( Bash
      , LangConfig
          { lcExtensions = [".sh", ".bash", ".zsh", ".fish"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = ": '"
          , lcMultiLineEnd = "'"
          , lcNestedComments = False
          })
    , ( Basic
      , LangConfig
          { lcExtensions = [".bas", ".vb", ".vbs"]
          , lcSingleLineComment = ["'", "REM"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( C
      , LangConfig
          { lcExtensions = [".c", ".h"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( COBOL
      , LangConfig
          { lcExtensions = [".cob", ".cbl", ".cpy"]
          , lcSingleLineComment = ["*"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( CPP
      , LangConfig
          { lcExtensions = [".cpp", ".hpp", ".cc", ".hxx", ".cxx"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( CSharp
      , LangConfig
          { lcExtensions = [".cs"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( CSS
      , LangConfig
          { lcExtensions = [".css"]
          , lcSingleLineComment = []
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( D
      , LangConfig
          { lcExtensions = [".d"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = True
          })
    , ( Dart
      , LangConfig
          { lcExtensions = [".dart"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( Elixir
      , LangConfig
          { lcExtensions = [".ex", ".exs"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "@doc \"\"\""
          , lcMultiLineEnd = "\"\"\""
          , lcNestedComments = False
          })
    , ( Elm
      , LangConfig
          { lcExtensions = [".elm"]
          , lcSingleLineComment = ["--"]
          , lcMultiLineStart = "{-"
          , lcMultiLineEnd = "-}"
          , lcNestedComments = True
          })
    , ( Erlang
      , LangConfig
          { lcExtensions = [".erl", ".hrl"]
          , lcSingleLineComment = ["%"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( FSharp
      , LangConfig
          { lcExtensions = [".fs", ".fsx", ".fsi"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "(*"
          , lcMultiLineEnd = "*)"
          , lcNestedComments = True
          })
    , ( Fortran
      , LangConfig
          { lcExtensions = [".f", ".f90", ".f95", ".f03"]
          , lcSingleLineComment = ["!"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( Go
      , LangConfig
          { lcExtensions = [".go"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( Groovy
      , LangConfig
          { lcExtensions = [".groovy", ".gvy", ".gy", ".gsh"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( Haskell
      , LangConfig
          { lcExtensions = [".hs", ".lhs"]
          , lcSingleLineComment = ["--"]
          , lcMultiLineStart = "{-"
          , lcMultiLineEnd = "-}"
          , lcNestedComments = True
          })
    , ( HTML
      , LangConfig
          { lcExtensions = [".html", ".htm", ".xhtml"]
          , lcSingleLineComment = []
          , lcMultiLineStart = "<!--"
          , lcMultiLineEnd = "-->"
          , lcNestedComments = False
          })
    , ( Java
      , LangConfig
          { lcExtensions = [".java"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( JavaScript
      , LangConfig
          { lcExtensions = [".js", ".jsx", ".mjs"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( JSON
      , LangConfig
          { lcExtensions = [".json"]
          , lcSingleLineComment = []
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( Julia
      , LangConfig
          { lcExtensions = [".jl"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "#="
          , lcMultiLineEnd = "=#"
          , lcNestedComments = True
          })
    , ( Kotlin
      , LangConfig
          { lcExtensions = [".kt", ".kts"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = True
          })
    , ( Lisp
      , LangConfig
          { lcExtensions = [".lisp", ".cl", ".el"]
          , lcSingleLineComment = [";"]
          , lcMultiLineStart = "#|"
          , lcMultiLineEnd = "|#"
          , lcNestedComments = True
          })
    , ( Lua
      , LangConfig
          { lcExtensions = [".lua"]
          , lcSingleLineComment = ["--"]
          , lcMultiLineStart = "--[["
          , lcMultiLineEnd = "]]"
          , lcNestedComments = False
          })
    , ( MATLAB
      , LangConfig
          { lcExtensions = [".m", ".mat"]
          , lcSingleLineComment = ["%"]
          , lcMultiLineStart = "%{"
          , lcMultiLineEnd = "%}"
          , lcNestedComments = False
          })
    , ( Nim
      , LangConfig
          { lcExtensions = [".nim"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "#["
          , lcMultiLineEnd = "]#"
          , lcNestedComments = True
          })
    , ( ObjectiveC
      , LangConfig
          { lcExtensions = [".m", ".mm"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( OCaml
      , LangConfig
          { lcExtensions = [".ml", ".mli"]
          , lcSingleLineComment = []
          , lcMultiLineStart = "(*"
          , lcMultiLineEnd = "*)"
          , lcNestedComments = True
          })
    , ( Pascal
      , LangConfig
          { lcExtensions = [".pas", ".pp"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "{"
          , lcMultiLineEnd = "}"
          , lcNestedComments = False
          })
    , ( Perl
      , LangConfig
          { lcExtensions = [".pl", ".pm", ".t"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "=pod"
          , lcMultiLineEnd = "=cut"
          , lcNestedComments = False
          })
    , ( PHP
      , LangConfig
          { lcExtensions = [".php"]
          , lcSingleLineComment = ["//", "#"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( PowerShell
      , LangConfig
          { lcExtensions = [".ps1", ".psm1", ".psd1"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "<#"
          , lcMultiLineEnd = "#>"
          , lcNestedComments = False
          })
    , ( Prolog
      , LangConfig
          { lcExtensions = [".pl", ".pro", ".p"]
          , lcSingleLineComment = ["%"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( Python
      , LangConfig
          { lcExtensions = [".py", ".pyw", ".pyx"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "\"\"\""
          , lcMultiLineEnd = "\"\"\""
          , lcNestedComments = False
          })
    , ( R
      , LangConfig
          { lcExtensions = [".r", ".R"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( Racket
      , LangConfig
          { lcExtensions = [".rkt"]
          , lcSingleLineComment = [";"]
          , lcMultiLineStart = "#|"
          , lcMultiLineEnd = "|#"
          , lcNestedComments = True
          })
    , ( Ruby
      , LangConfig
          { lcExtensions = [".rb", ".rake", ".gemspec"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = "=begin"
          , lcMultiLineEnd = "=end"
          , lcNestedComments = False
          })
    , ( Rust
      , LangConfig
          { lcExtensions = [".rs"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = True
          })
    , ( Scala
      , LangConfig
          { lcExtensions = [".scala", ".sc"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = True
          })
    , ( Scheme
      , LangConfig
          { lcExtensions = [".scm", ".ss"]
          , lcSingleLineComment = [";"]
          , lcMultiLineStart = "#|"
          , lcMultiLineEnd = "|#"
          , lcNestedComments = True
          })
    , ( Shell
      , LangConfig
          { lcExtensions = [".sh", ".bash", ".zsh", ".fish"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = ": '"
          , lcMultiLineEnd = "'"
          , lcNestedComments = False
          })
    , ( Smalltalk
      , LangConfig
          { lcExtensions = [".st"]
          , lcSingleLineComment = ["\""]
          , lcMultiLineStart = "\""
          , lcMultiLineEnd = "\""
          , lcNestedComments = False
          })
    , ( SQL
      , LangConfig
          { lcExtensions = [".sql"]
          , lcSingleLineComment = ["--"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( Swift
      , LangConfig
          { lcExtensions = [".swift"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = True
          })
    , ( Tcl
      , LangConfig
          { lcExtensions = [".tcl"]
          , lcSingleLineComment = ["#"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( TypeScript
      , LangConfig
          { lcExtensions = [".ts", ".tsx"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = "/*"
          , lcMultiLineEnd = "*/"
          , lcNestedComments = False
          })
    , ( VB
      , LangConfig
          { lcExtensions = [".vb"]
          , lcSingleLineComment = ["'"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( VHDL
      , LangConfig
          { lcExtensions = [".vhd", ".vhdl"]
          , lcSingleLineComment = ["--"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    , ( XML
      , LangConfig
          { lcExtensions = [".xml", ".svg", ".plist"]
          , lcSingleLineComment = []
          , lcMultiLineStart = "<!--"
          , lcMultiLineEnd = "-->"
          , lcNestedComments = False
          })
    , ( Zig
      , LangConfig
          { lcExtensions = [".zig"]
          , lcSingleLineComment = ["//"]
          , lcMultiLineStart = ""
          , lcMultiLineEnd = ""
          , lcNestedComments = False
          })
    ]

data ParserState = ParserState
  { psLanguage     :: Language
  , psInComment    :: Bool
  , psCommentDepth :: Int
  } deriving (Show)

processFile :: FilePath -> IO FileStats
processFile path = do
  content <- BS.readFile path -- Use ByteString for faster IO
  case TE.decodeUtf8' content of
    Left _ -> return emptyStats
    Right text ->
      case detectLanguage path of
        Nothing -> return emptyStats
        Just lang -> do
          let linesa = T.lines text -- Pre-split into lines
              stats = processLines lang linesa
          return stats -- Ensure strict evaluation

processLines :: Language -> [Text] -> FileStats
processLines lang = foldl' (processLine lang) emptyStats

processLine :: Language -> FileStats -> Text -> FileStats
processLine lang stats line =
  let trimmed = T.strip line
      isBlank = T.null trimmed
   in case lang of
        Other _ ->
          if isBlank
            then stats {fsBlankLines = fsBlankLines stats + 1}
            else stats {fsCodeLines = fsCodeLines stats + 1}
        _ ->
          let config = languageConfigs Map.! lang
              isSingleComment =
                any (`T.isPrefixOf` trimmed) (lcSingleLineComment config)
              isMultiStart = lcMultiLineStart config `T.isPrefixOf` trimmed
           in if isBlank
                then stats {fsBlankLines = fsBlankLines stats + 1}
                else if isSingleComment
                       then stats
                              {fsSingleComments = fsSingleComments stats + 1}
                       else if isMultiStart
                              then stats
                                     { fsMultiComments =
                                         fsMultiComments stats + 1
                                     }
                              else stats {fsCodeLines = fsCodeLines stats + 1}

processDirectory :: ProgressBar () -> FilePath -> IO (Map Language FileStats)
processDirectory pb path = do
  contents <- listDirectory path
  foldM (processEntry pb) Map.empty contents
  where
    processEntry pbs acc name = do
      let fullPath = path </> name
      isFile <- doesFileExist fullPath
      isDir <- doesDirectoryExist fullPath
      case (isFile, isDir) of
        (True, _) -> do
          case detectLanguage fullPath of
            Nothing -> return acc
            Just lang -> do
              stats <- processFile fullPath
              updateProgress pbs $ \p -> p {progressDone = progressDone p + 1}
              return $! Map.insertWith combineStats lang stats acc
        (_, True) -> do
          subStats <- processDirectory pbs fullPath
          return $! Map.unionWith combineStats acc subStats
        _ -> return acc

countLines :: FilePath -> Bool -> IO (Map Language FileStats)
countLines path verbose = do
  verboseLog verbose "Starting line counting process..."
  totalFiles <- countSourceFiles path
  verboseLogF verbose "INIT"
    $ "Found " ++ show totalFiles ++ " source files to process"
  let style =
        defStyle
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
  verboseLogF verbose "PATH"
    $ "Path type: "
        ++ if isFile
             then "File"
             else if isDir
                    then "Directory"
                    else "Unknown"
  result <-
    case (isFile, isDir) of
      (True, _) -> do
        verboseLogF verbose "FILE" $ "Processing single file: " ++ path
        stats <- processFile path
        updateProgress pb $ \p -> p {progressDone = progressDone p + 1}
        case detectLanguage path of
          Nothing -> do
            verboseLogF verbose "FILE" "Could not detect language"
            return Map.empty
          Just lang -> do
            verboseLogF verbose "FILE" $ "Detected language: " ++ show lang
            return $ Map.singleton lang stats
      (_, True) -> do
        verboseLogF verbose "DIR" $ "Processing directory: " ++ path
        processDirectory pb path
      _ -> do
        verboseLogF verbose "ERROR" $ "Path does not exist: " ++ path
        return Map.empty
  verboseLog verbose "Processing complete!"
  hideProgressBar
  putStrLn ""
  return result

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
  let (others, knowns) = Map.partitionWithKey (\k _ -> isOther k) stats
  mapM_ printLangStats (Map.toList knowns)
  unless (Map.null others) $ printOtherStats (Map.elems others)
  printTotalStats (foldl combineStats emptyStats (Map.elems stats))
  setSGR [Reset]
  where
    isOther (Other _) = True
    isOther _         = False
    printLangStats :: (Language, FileStats) -> IO ()
    printLangStats (lang, FileStats {..}) =
      let langStr = take 12 $ show lang ++ replicate 12 ' '
       in do
            setSGR
              [ SetConsoleIntensity BoldIntensity
              , SetColor Foreground Vivid (languageColor lang)
              ]
            printf
              "%-12s %7d %8d %9d %8d\n"
              langStr
              fsFileCount
              fsBlankLines
              (fsSingleComments + fsMultiComments)
              fsCodeLines
    printOtherStats :: [FileStats] -> IO ()
    printOtherStats statsa =
      let combined = foldl combineStats emptyStats statsa
       in do
            setSGR
              [ SetConsoleIntensity BoldIntensity
              , SetColor Foreground Vivid White
              ]
            printf
              "%-12s %7d %8d %9d %8d\n"
              ("Other" :: String)
              (fsFileCount combined)
              (fsBlankLines combined)
              (fsSingleComments combined + fsMultiComments combined)
              (fsCodeLines combined)
    printTotalStats :: FileStats -> IO ()
    printTotalStats FileStats {..} =
      let
       in do
            setSGR
              [ SetConsoleIntensity BoldIntensity
              , SetColor Foreground Vivid White
              ]
            putStrLn "-----------------------------------------------"
            printf
              "%-12s %7d %8d %9d %8d\n"
              ("Total" :: String)
              fsFileCount
              fsBlankLines
              (fsSingleComments + fsMultiComments)
              fsCodeLines

combineStats :: FileStats -> FileStats -> FileStats
combineStats (FileStats b1 c1 s1 m1 f1) (FileStats b2 c2 s2 m2 f2) =
  let !b = b1 + b2
      !c = c1 + c2
      !s = s1 + s2
      !m = m1 + m2
      !f = f1 + f2
   in FileStats b c s m f
