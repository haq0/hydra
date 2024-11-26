{-# LANGUAGE OverloadedStrings #-}

module Utils.License
  ( detectLicense
  , License(..)
  , LicenseInfo(..)
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import Data.Char (toLower)
import Data.List (find, maximumBy)
import Control.Monad (filterM, forM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Function (on)

data License
  = MIT
  | Apache2
  | GPL2
  | GPL3
  | BSD2
  | BSD3
  | LGPL
  | MPL2
  | AGPL3
  | Unlicense
  | ISC
  | Custom String
  | Unknown
  deriving (Show, Eq, Ord)

data LicenseInfo = LicenseInfo
  { liLicense :: License
  , liFile    :: FilePath
  , liConfidence :: Double
  } deriving (Show, Eq)

licenseFiles :: [String]
licenseFiles =
  [ "license"
  , "licence"
  , "copying"
  , "copyright"
  , "LICENSE"
  , "LICENCE"
  , "COPYING"
  , "COPYRIGHT"
  , "License"
  , "Licence"
  , "license.txt"
  , "licence.txt"
  , "LICENSE.txt"
  , "LICENCE.txt"
  , "license.md"
  , "licence.md"
  , "LICENSE.md"
  , "LICENCE.md"
  , "COPYING.txt"
  , "COPYING.md"
  , "copying.txt"
  , "copying.md"
  , "LICENSE.rst"
  , "license.rst"
  , "UNLICENSE"
  , "UNLICENCE"
  , "unlicense"
  , "unlicence"
  , "COPYRIGHT.txt"
  , "Copyright.txt"
  , "copyright.txt"
  , "LICENSE-MIT"
  , "LICENSE-APACHE"
  , "LICENSE.MIT"
  , "LICENSE.APACHE"
  , "MIT-LICENSE"
  , "APACHE-LICENSE"
  , "LICENSE-BSD"
  , "BSD-LICENSE"
  ]
licenseFingerprints :: Map License [T.Text]
licenseFingerprints = Map.fromList
  [ (MIT, 
     [ "mit license"
     , "permission is hereby granted, free of charge"
     , "without restriction, including without limitation"
     , "mit © "
     ])
  , (Apache2,
     [ "apache license, version 2"
     , "apache license 2"
     , "licensed under the apache license"
     , "www.apache.org/licenses"
     , "apache-2.0"
     ])
  , (GPL3,
     [ "gnu general public license version 3"
     , "gpl version 3"
     , "gpl-3"
     , "www.gnu.org/licenses/gpl-3"
     , "either version 3 of the license"
     ])
  , (GPL2,
     [ "gnu general public license version 2"
     , "gpl version 2"
     , "gpl-2"
     , "www.gnu.org/licenses/gpl-2"
     , "either version 2 of the license"
     ])
  , (BSD3,
     [ "bsd 3-clause"
     , "redistribution and use in source and binary"
     , "3. neither the name"
     , "bsd-3-clause"
     ])
  , (BSD2,
     [ "bsd 2-clause"
     , "redistribution and use in source and binary"
     , "2. redistributions in binary"
     , "bsd-2-clause"
     ])
  , (LGPL,
     [ "gnu lesser general public license"
     , "gnu library general public license"
     , "www.gnu.org/licenses/lgpl"
     , "lesser general public license"
     , "either version 3 of the license"
     ])
  , (MPL2,
     [ "mozilla public license version 2"
     , "mozilla public license 2"
     , "mpl-2.0"
     , "mozilla.org/MPL/2.0"
     ])
  , (AGPL3,
     [ "gnu affero general public license"
     , "agpl version 3"
     , "gnu agpl version 3"
     , "www.gnu.org/licenses/agpl"
     , "agpl-3.0"
     ])
  , (Unlicense,
     [ "this is free and unencumbered software"
     , "unlicense"
     , "unlicense.org"
     ])
  , (ISC,
     [ "isc license"
     , "permission to use, copy, modify, and/or distribute this software"
     , "the software is provided \"as is\""
     , "internet systems consortium"
     ])
  ]
findLicenseFiles :: FilePath -> IO [FilePath]
findLicenseFiles dir = do
  files <- listDirectory dir
  let potentialFiles = filter isLicenseFile files
  filterM (doesFileExist . (dir </>)) potentialFiles
  where
    isLicenseFile f = 
      let loweredName = map toLower (takeFileName f)
      in any (\pattern -> map toLower pattern == loweredName) licenseFiles

calculateConfidence :: License -> T.Text -> Double
calculateConfidence license content =
  case Map.lookup license licenseFingerprints of
    Nothing -> 0.0
    Just fingerprints ->
      let normalized = T.toLower $ normalizeLicenseText content
          matches = length $ filter (`T.isInfixOf` normalized) fingerprints
          total = length fingerprints
      in if matches > 0
         then min 1.0 $ (fromIntegral matches / fromIntegral total) * 1.5  
         else 0.0

normalizeLicenseText :: T.Text -> T.Text
normalizeLicenseText = T.toLower . T.strip . T.filter (`notElem` (",.()[]{}\"'" :: String))
detectLicenseFromContent :: T.Text -> (License, Double)
detectLicenseFromContent content =
  let normalized = T.toLower $ normalizeLicenseText content
      matches = Map.mapWithKey (\k _ -> calculateConfidence k normalized) licenseFingerprints
      filteredMatches = filter ((> 0.3) . snd) (Map.toList matches)
  in case findBestMatch filteredMatches of
       Just (license, _) -> (license, 1.0)  
       Nothing -> 
         if not (T.null content)
         then (Custom "Unknown License", 0.0)
         else (Unknown, 0.0)
  where
    findBestMatch [] = Nothing
    findBestMatch xs = Just $ maximumBy (compare `on` snd) xs


detectLicense :: FilePath -> IO [LicenseInfo]
detectLicense dir = do
  files <- findLicenseFiles dir
  explicitLicenses <- forM files $ \file -> do
    content <- TIO.readFile (dir </> file)
    let (license, confidence) = detectLicenseFromContent content
    return $ LicenseInfo
      { liLicense = license
      , liFile = file
      , liConfidence = confidence
      }

  mbPackageLicense <- checkPackageManagerFiles dir
  let packageLicenses = case mbPackageLicense of
        Just license -> 
          [LicenseInfo 
            { liLicense = license
            , liFile = "package manager files"
            , liConfidence = 0.8
            }]
        Nothing -> []

  return $ explicitLicenses ++ packageLicenses
checkPackageManagerFiles :: FilePath -> IO (Maybe License)
checkPackageManagerFiles dir = do
  let packageFiles = 
        [ "package.json"      -- npm
        , "composer.json"     -- PHP
        , "*.gemspec"         -- Ruby
        , "setup.py"          -- Python
        , "cargo.toml"        -- Rust
        , "*.cabal"          -- Haskell
        , "build.gradle"      -- Gradle
        , "pom.xml"          -- Maven
        , "build.sbt"        -- Scala
        , "mix.exs"          -- Elixir
        ]

  -- Implementation would parse these files for license fields
  return Nothing
