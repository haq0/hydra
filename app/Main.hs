{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}



module Main where

import           Control.Monad
import           Control.Exception
import qualified Data.Map.Strict     as Map
import           Lib
import           Stats.Processing
import           Options.Applicative
import           System.Console.ANSI (hideCursor, showCursor)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                     removeDirectoryRecursive)
import           System.Exit        (exitFailure)
import           Text.Printf
import           System.IO          (hPutStrLn, stderr)
import           System.IO.Temp     (withSystemTempDirectory)
import           System.Process     (callProcess, readCreateProcess, shell)
import           Data.Maybe         (mapMaybe)

data Options = Options
  { optVerbose      :: Bool
  , optGit          :: Bool
  , optPath         :: FilePath
  , optIgnoreDirs   :: [FilePath]
  , optIgnoreLangs  :: [String]
  , optIgnoreHidden :: Bool
  , optDetectLicense :: Bool
  } deriving (Show)

stringToLanguage :: String -> Maybe Language
stringToLanguage = \case
    "ada" -> Just Ada
    "asm" -> Just Assembly
    "awk" -> Just AWK
    "bash" -> Just Bash
    "c" -> Just C
    "cpp" -> Just CPP
    "cs" -> Just CSharp
    "css" -> Just CSS
    "go" -> Just Go
    "hs" -> Just Haskell
    "html" -> Just HTML
    "java" -> Just Java
    "js" -> Just JavaScript
    "json" -> Just JSON
    "py" -> Just Python
    "rb" -> Just Ruby
    "rs" -> Just Rust
    "scala" -> Just Scala
    "sql" -> Just SQL
    "swift" -> Just Swift
    "ts" -> Just TypeScript
    "xml" -> Just XML
    other -> Just (Other other)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch 
        (long "verbose" 
         <> short 'v' 
         <> help "Enable verbose output")
    <*> switch 
        (long "git" 
         <> help "Clone a Git repository from the provided URL")
    <*> argument str
        (metavar "PATH" 
         <> help "Path to analyze or URL of the repository if --git is specified")
    <*> many 
        (strOption
          (long "ignore-dir"
           <> metavar "DIR"
           <> help "Directory to ignore (can be specified multiple times)"))
    <*> many 
        (strOption
          (long "ignore-lang"
           <> metavar "LANG"
           <> help "Language to ignore (can be specified multiple times, e.g. js,py,rb)"))
    <*> switch
        (long "ignore-hidden"
         <> help "Ignore hidden directories (starting with .)")
    <*> switch
        (long "detect-license"
          <> short 'l'
          <> help "Detect software licenses in the codebase")

opts :: ParserInfo Options
opts =
  info
    (parseOptions <**> helper)
    (fullDesc
     <> progDesc "Count lines of code in source files or a Git repository"
     <> header "hydra - a source code line counter")

makeConfig :: Options -> FilePath -> Config
makeConfig Options{..} path = Config 
  { configPath = path
  , configVerbose = optVerbose
  , configIgnoreDirs = defaultIgnoreDirs ++ optIgnoreDirs ++ hiddenDirs
  , configIgnoreLangs = mapMaybe stringToLanguage optIgnoreLangs
  }
  where
    defaultIgnoreDirs = ["dist", "dist-newstyle", ".stack-work", "node_modules"]
    hiddenDirs = if optIgnoreHidden then [".git", ".hie", ".vscode"] else []
main :: IO ()
main = do
  options <- execParser opts
  hideCursor

  flip finally showCursor $ do
    if optGit options
      then withSystemTempDirectory "hydra-clone" $ \tempDir -> do
        when (optVerbose options) $ 
          putStrLn $ "Cloning repository: " ++ optPath options

        let gitCommand = "git clone " ++ optPath options ++ " " ++ tempDir
        if optVerbose options
          then callProcess "git" ["clone", optPath options, tempDir]
          else do
            _ <- readCreateProcess
                   (shell $ gitCommand ++ " > /dev/null 2>&1")
                   ""
            return ()

        when (optVerbose options) $ 
          putStrLn $ "Repository cloned to: " ++ tempDir

        when (optDetectLicense options) $ do
          when (optVerbose options) $
            putStrLn "Scanning for licenses..."
          licenses <- detectLicense tempDir
          unless (null licenses) $ do
            putStrLn "\nDetected Licenses:"
            putStrLn "-------------------"
            forM_ licenses $ \LicenseInfo{..} -> do
              putStrLn $ printf "%-12s (%.0f%% confidence) in %s"
                (show liLicense)
                (liConfidence * 100)
                liFile
            putStrLn ""

        results <- countLines (makeConfig options tempDir)
        hideProgressBar

        if Map.null results
          then do
            putStrLn "No source files found to analyze"
            exitFailure
          else prettyPrint results

        when (optVerbose options) $
          putStrLn $ "Cleaning up temporary directory: " ++ tempDir
        removeDirectoryRecursive tempDir

      else do
        when (optVerbose options) $ 
          putStrLn $ "Analyzing path: " ++ optPath options

        isFile <- doesFileExist (optPath options)
        isDir <- doesDirectoryExist (optPath options)

        unless (isFile || isDir) $ do
          hPutStrLn stderr $ "Error: Path does not exist: " ++ optPath options
          exitFailure

        when (optDetectLicense options) $ do
          when (optVerbose options) $
            putStrLn "Scanning for licenses..."
          licenses <- detectLicense (optPath options)
          unless (null licenses) $ do
            putStrLn "\nDetected Licenses:"
            putStrLn "-------------------"
            forM_ licenses $ \LicenseInfo{..} -> do
              putStrLn $ printf "%-12s (%.0f%% confidence) in %s"
                (show liLicense)
                (liConfidence * 100)
                liFile
            putStrLn ""

        results <- countLines (makeConfig options (optPath options))
        hideProgressBar

        if Map.null results
          then do
            putStrLn "No source files found to analyze"
            exitFailure
          else prettyPrint results

