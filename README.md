# Hydra - Source Code Line Counter

Hydra is a fast and accurate source code line counter written in Haskell that supports multiple programming languages and provides detailed statistics about your codebase.

## Features

- Supports multiple programming languages:
  - Ada
  - Assembly
  - AWK
  - Bash/Shell scripts
  - Basic/VB
  - C
  - COBOL
  - C++
  - C#
  - CSS
  - D
  - Dart
  - Elixir
  - Elm
  - Erlang
  - F#
  - Fortran
  - Go
  - Groovy
  - Haskell
  - HTML
  - Java
  - JavaScript
  - JSON
  - Julia
  - Kotlin
  - Lisp
  - Lua
  - MATLAB
  - Nim
  - Objective-C
  - OCaml
  - Pascal
  - Perl
  - PHP
  - PowerShell
  - Prolog
  - Python
  - R
  - Racket
  - Ruby
  - Rust
  - Scala
  - Scheme
  - Smalltalk
  - SQL
  - Swift
  - Tcl
  - TypeScript
  - VHDL
  - XML
  - Zig

- Counts:
  - Code lines
  - Blank lines
  - Single-line comments
  - Multi-line comments
  - Total files

- Additional features:
  - Progress bar
  - Direct Git repository analysis
  - Colored output
  - Verbose logging mode
  - Ignore filters based on directories, hidden directories, or languages.
  - License detection

## Installation

Simply run `stack install` to build and install Hydra.

## Usage

Basic usage: `hydra [OPTIONS] PATH`

Options:
- `-v, --verbose`: Enable verbose output
- `--git`: Clone and analyze a Git repository
- `-h, --help`: Show help message
- `--ignore-hidden`: Ignores hidden directories
- `--ignore-dir DIR`: Ignores specified directories
- `--ignore-lang LANG`: Ignores specified languages
- `-l`, `--detect-license`: Detects the license (beta)

Examples:
- Analyze local directory: `hydra ./my-project`
- Analyze with verbose output: `hydra -v ./my-project`
- Analyze Git repository: `hydra --git https://github.com/haq0/repo`
- Show help: `hydra --help`

## Output Example

When running Hydra, you'll see output like this:
```bash
Summary:
Language      Files    Blank   Comment     Code
-----------------------------------------------
Haskell           2      150       200      500
Python            5       80       120      300
JavaScript        3       40        60      200
-----------------------------------------------
Total            10      270       380     1000
```

## Supported Comment Styles

- Single-line comments: `//`, `#`, `--`
- Multi-line comments: `/* */`, `{- -}`, `"""`, etc.
- Nested comments (for supported languages)

## Contributing

Contributions are welcome! Feel free to:
- Report bugs
- Suggest features
- Add support for new languages
- Improve documentation

## License

[GNU AFFERO GENERAL PUBLIC LICENSE V3.0](https://www.gnu.org/licenses/agpl-3.0.en.html)
