{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Options.Applicative
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Control.Monad (when)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist, doesFileExist)
import System.Process (callProcess, readCreateProcess, shell)
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hPutStrLn, stderr)
import System.Console.ANSI (hideCursor, showCursor)
data Options = Options
    { optVerbose :: Bool
    , optGit :: Bool
    , optPath :: FilePath
    } deriving (Show)

parseOptions :: Options.Applicative.Parser Options
parseOptions = Options
    <$> switch
        ( long "verbose"
        <> short 'v'
        <> help "Enable verbose output" )
    <*> switch
        ( long "git"
        <> help "Clone a Git repository from the provided URL" )
    <*> argument str
        ( metavar "PATH"
        <> help "Path to analyze or URL of the repository if --git is specified" )

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
    ( fullDesc
    <> progDesc "Count lines of code in source files or a Git repository"
    <> header "hydra - a source code line counter" )
main :: IO ()
main = do
    options <- execParser opts
    hideCursor  

    let verbose = optVerbose options

    if optGit options
        then withSystemTempDirectory "repo-clone" $ \tempDir -> do
            when verbose $
                putStrLn $ "Cloning repository: " ++ optPath options

            let gitCommand = "git clone " ++ optPath options ++ " " ++ tempDir
            if verbose
                then callProcess "git" ["clone", optPath options, tempDir]
                else do _ <- readCreateProcess (shell $ gitCommand ++ " > /dev/null 2>&1") ""
                        return ()

            when verbose $
                putStrLn $ "Repository cloned to: " ++ tempDir

            results <- countLines tempDir verbose

            
            hideProgressBar

            if Map.null results
                then do
                    putStrLn "No source files found to analyze"
                    exitFailure
                else prettyPrint results

            when verbose $
                putStrLn $ "Cleaning up temporary directory: " ++ tempDir

            removeDirectoryRecursive tempDir
        else do
            when verbose $
                putStrLn $ "Analyzing path: " ++ optPath options

            isFile <- doesFileExist (optPath options)
            isDir <- doesDirectoryExist (optPath options)

            if not (isFile || isDir)
                then do
                    hPutStrLn stderr $ "Error: Path does not exist: " ++ optPath options
                    exitFailure
                else do
                    results <- countLines (optPath options) verbose

                    hideProgressBar

                    if Map.null results
                        then do
                            putStrLn "No source files found to analyze"
                            exitFailure
                        else prettyPrint results
    showCursor  

