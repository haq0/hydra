{-# LANGUAGE OverloadedStrings #-}

module Utils.Filter
  ( shouldProcessPath
  , shouldProcessLanguage
  ) where

import           Data.List         (isInfixOf)
import           Language.Language (Language)
import           System.FilePath   (normalise, splitPath)

shouldProcessPath :: [FilePath] -> FilePath -> Bool
shouldProcessPath ignoreDirs path =
  let normalizedPath = normalise path
      pathParts = splitPath normalizedPath
   in not $ any (\dir -> dir `isInfixOf` normalizedPath) ignoreDirs

shouldProcessLanguage :: [Language] -> Language -> Bool
shouldProcessLanguage ignoreLangs lang = lang `notElem` ignoreLangs
