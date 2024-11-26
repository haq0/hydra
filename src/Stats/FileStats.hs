{-# LANGUAGE BangPatterns #-}

module Stats.FileStats
  ( FileStats(..)
  , emptyStats
  , combineStats
  ) where

data FileStats = FileStats
  { fsBlankLines     :: {-# UNPACK #-} !Int
  , fsCodeLines      :: {-# UNPACK #-} !Int
  , fsSingleComments :: {-# UNPACK #-} !Int
  , fsMultiComments  :: {-# UNPACK #-} !Int
  , fsFileCount      :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

emptyStats :: FileStats
emptyStats = FileStats 0 0 0 0 1

combineStats :: FileStats -> FileStats -> FileStats
combineStats (FileStats b1 c1 s1 m1 f1) (FileStats b2 c2 s2 m2 f2) =
  let !b = b1 + b2
      !c = c1 + c2
      !s = s1 + s2
      !m = m1 + m2
      !f = f1 + f2
   in FileStats b c s m f
