# Python Parser in F# using ANTLR

## Overview
This project is an F# application that parses Python source code using ANTLR (ANother Tool for Language Recognition). It can validate Python syntax, generate parse trees, and display token information for Python files.

## Project Structure
```
src/PythonParser/
  PythonParser.sln                 # Solution file
  PythonParser/                    # F# console application
    Program.fs                     # Main entry point and CLI
    PythonParser.fsproj            # F# project file
    Grammar/                       # ANTLR grammar files (.g4)
  PythonParser.Grammar/            # C# class library for ANTLR-generated code
    Python3Lexer.cs                # Generated lexer
    Python3Parser.cs               # Generated parser
    Python3LexerBase.cs            # Lexer base class
    Python3ParserBase.cs           # Parser base class
    ...                            # Other generated files
examples/
  sample.py                        # Sample valid Python file
  syntax_error.py                  # Sample file with syntax errors
```

## Technology Stack
- **F#**: Main application logic and CLI
- **C#**: ANTLR-generated parser/lexer code
- **.NET 8.0**: Runtime framework
- **ANTLR 4.13.1**: Parser generator
- **Python3 Grammar**: Full Python 3 grammar from official ANTLR grammars repository

## Usage

### Parse a Python file
```bash
cd src/PythonParser/PythonParser
dotnet run -- /path/to/file.py
```

### Parse inline Python code
```bash
dotnet run -- --code "print('Hello, World!')"
```

### Show tokens for a file
```bash
dotnet run -- --tokens /path/to/file.py
```

### Display help
```bash
dotnet run -- --help
```

## Building the Project
```bash
cd src/PythonParser
dotnet build
```

## Architecture Decisions
- **Mixed-language solution**: F# projects cannot directly include C# source files, so the ANTLR-generated C# code lives in a separate class library (PythonParser.Grammar) that the F# project references.
- **Full Python 3 Grammar**: Uses the complete Python3 grammar from the official ANTLR grammars-v4 repository, supporting all modern Python syntax.
- **Error listener**: Custom error listener collects all syntax errors with line/column information for user-friendly reporting.

## Recent Changes
- 2025-12-08: Initial project creation with ANTLR Python parser
