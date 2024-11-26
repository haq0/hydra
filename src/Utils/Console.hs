{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Utils.Console
  ( verboseLog
  , verboseLogF
  , hideProgressBar
  , prettyPrint
  ) where

import           Control.Monad       (unless, when)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           System.Console.ANSI
import           System.IO           (hFlush, stdout)
import           Text.Printf         (printf)

import           Language.Language   (Language (..), languageColor)
import           Stats.FileStats     (FileStats (..), combineStats, emptyStats)

verboseLog :: Bool -> String -> IO ()
verboseLog verbose msg =
  when verbose $ do
    putStrLn $ "\x1b[36m[DEBUG]\x1b[0m " ++ msg
    hFlush stdout

verboseLogF :: Bool -> String -> String -> IO ()
verboseLogF verbose prefix msg =
  when verbose $ do
    putStrLn $ "\x1b[36m[DEBUG:" ++ prefix ++ "]\x1b[0m " ++ msg
    hFlush stdout

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
    printOtherStats stats =
      let combined = foldl combineStats emptyStats stats
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
    printTotalStats FileStats {..} = do
      setSGR
        [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
      putStrLn "-----------------------------------------------"
      printf
        "%-12s %7d %8d %9d %8d\n"
        ("Total" :: String)
        fsFileCount
        fsBlankLines
        (fsSingleComments + fsMultiComments)
        fsCodeLines
