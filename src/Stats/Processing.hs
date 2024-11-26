{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stats.Processing
  ( processFile
  , processDirectory
  , Config(..)
  , countSourceFiles
  ) where

import           Control.Monad      (foldM, forM)
import qualified Data.ByteString    as BS
import           Data.List          (foldl')
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           System.Directory   (doesDirectoryExist, doesFileExist,
                                     listDirectory)
import           System.FilePath    ((</>))
import           System.ProgressBar (Progress (..), ProgressBar, updateProgress)

import           Language.Language  (LangConfig (..), Language (..),
                                     detectLanguage, languageConfigs)
import           Stats.FileStats    (FileStats (..), combineStats, emptyStats)
import Utils.Filter

data Config = Config
  { configPath          :: FilePath
  , configVerbose       :: Bool
  , configIgnoreDirs    :: [FilePath]
  , configIgnoreLangs   :: [Language]
  } deriving (Show)

processFile :: FilePath -> IO FileStats
processFile path = do
  content <- BS.readFile path
  case TE.decodeUtf8' content of
    Left _ -> return emptyStats
    Right text ->
      case detectLanguage path of
        Nothing -> return emptyStats
        Just lang -> do
          let lines = T.lines text
              stats = processLines lang lines
          return stats

processLines :: Language -> [T.Text] -> FileStats
processLines lang = foldl' (processLine lang) emptyStats

processLine :: Language -> FileStats -> T.Text -> FileStats
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

processDirectory :: Config -> ProgressBar () -> FilePath -> IO (Map Language FileStats)
processDirectory Config{..} pb path = do
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
            Just lang -> 
              if shouldProcessPath configIgnoreDirs fullPath && 
                 shouldProcessLanguage configIgnoreLangs lang
                then do
                  stats <- processFile fullPath
                  updateProgress pbs $ \p -> p {progressDone = progressDone p + 1}
                  return $! Map.insertWith combineStats lang stats acc
                else return acc
        (_, True) -> 
          if shouldProcessPath configIgnoreDirs fullPath
            then do
              subStats <- processDirectory Config{..} pbs fullPath
              return $! Map.unionWith combineStats acc subStats
            else return acc
        _ -> return acc
countSourceFiles :: Config -> FilePath -> IO Int
countSourceFiles Config{..} path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, _) ->
      case detectLanguage path of
        Just lang -> 
          if shouldProcessPath configIgnoreDirs path && 
             shouldProcessLanguage configIgnoreLangs lang
            then return 1
            else return 0
        Nothing -> return 0
    (_, True) -> 
      if shouldProcessPath configIgnoreDirs path
        then do
          contents <- listDirectory path
          counts <- forM contents $ \name -> 
            countSourceFiles Config{..} (path </> name)
          return $ sum counts
        else return 0
    _ -> return 0
