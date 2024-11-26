{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Language
  ( Language(..)
  , LangConfig(..)
  , languageColor
  , detectLanguage
  , detectOtherLanguage
  , languageConfigs
  ) where

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import           System.Console.ANSI (Color (..))
import           System.FilePath     (takeExtension)
import           Text.Megaparsec     ((<|>))

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

data LangConfig = LangConfig
  { lcExtensions        :: [String]
  , lcSingleLineComment :: [Text]
  , lcMultiLineStart    :: Text
  , lcMultiLineEnd      :: Text
  , lcNestedComments    :: Bool
  }

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
