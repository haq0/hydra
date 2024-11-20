# Hydra - Source Code Line Counter

Hydra is a fast and accurate source code line counter written in Haskell that supports multiple programming languages and provides detailed statistics about your codebase.

## Features

- Supports multiple programming languages:
  - Haskell
  - Python
  - C/C++
  - JavaScript
  - Java
  - Ruby
  - Rust
  - Go
  - Swift
  - Kotlin
  - PHP
  - Shell scripts
  - HTML
  - CSS
  - SQL

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

## Installation

Simply run `stack install` to build and install Hydra.

## Usage

Basic usage: `hydra [OPTIONS] PATH`

Options:
- `-v, --verbose`: Enable verbose output
- `--git`: Clone and analyze a Git repository
- `-h, --help`: Show help message

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

(GNU AFFERO GENERAL PUBLIC LICENSE V3.0)[https://www.gnu.org/licenses/agpl-3.0.en.html]