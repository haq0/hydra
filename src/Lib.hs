{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( countLines
  , detectLicense
  , License(..)
  , LicenseInfo(..)
  , prettyPrint
  , FileStats(..)
  , Language(..)
  , hideProgressBar
  , verboseLog
  , verboseLogF
  ) where

import qualified Data.Map.Strict    as Map
import           System.Directory   (doesDirectoryExist, doesFileExist)
import           System.ProgressBar

import           Language.Language
import           Stats.FileStats
import           Stats.Processing
import           Utils.Console
import           Utils.Parser
import           Utils.Filter
import           Utils.License


countLines :: Config -> IO (Map.Map Language FileStats)
countLines config@Config{..} = do
  verboseLog configVerbose "Starting line counting process..."
  totalFiles <- countSourceFiles config configPath
  verboseLogF configVerbose "INIT"
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
  verboseLog configVerbose "Creating progress bar..."
  pb <- newProgressBar style 30 (Progress 0 totalFiles ())
  isFile <- doesFileExist configPath
  isDir <- doesDirectoryExist configPath
  verboseLogF configVerbose "PATH"
    $ "Path type: "
        ++ if isFile
             then "File"
             else if isDir
                    then "Directory"
                    else "Unknown"
  result <-
    case (isFile, isDir) of
      (True, _) -> do
        verboseLogF configVerbose "FILE" $ "Processing single file: " ++ configPath
        stats <- processFile configPath
        updateProgress pb $ \p -> p {progressDone = progressDone p + 1}
        case detectLanguage configPath of
          Nothing -> do
            verboseLogF configVerbose "FILE" "Could not detect language"
            return Map.empty
          Just lang -> 
            if shouldProcessLanguage configIgnoreLangs lang
              then do
                verboseLogF configVerbose "FILE" $ "Detected language: " ++ show lang
                return $ Map.singleton lang stats
              else return Map.empty
      (_, True) -> do
        verboseLogF configVerbose "DIR" $ "Processing directory: " ++ configPath
        processDirectory config pb configPath
      _ -> do
        verboseLogF configVerbose "ERROR" $ "Path does not exist: " ++ configPath
        return Map.empty
  verboseLog configVerbose "Processing complete!"
  hideProgressBar
  putStrLn ""
  return result

