module Utils.Parser
  ( LineType(..)
  , CommentType(..)
  , ParserState(..)
  ) where

import           Language.Language (Language)

data LineType
  = BlankLine
  | CodeLine Language
  | CommentLine CommentType
  deriving (Show, Eq)

data CommentType
  = SingleLine
  | MultiLine
  deriving (Show, Eq)

data ParserState = ParserState
  { psLanguage     :: Language
  , psInComment    :: Bool
  , psCommentDepth :: Int
  } deriving (Show)
