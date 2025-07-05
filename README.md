# COBOL AST Parser

A TypeScript-based COBOL Abstract Syntax Tree (AST) parser for analyzing COBOL source code structure.

This is an experiment to build an AST parser for COBOL using GitHub Copilot as a tutor, following a structured multi-phase implementation approach.

## Phase 1: Project Setup and Basic Infrastructure ✅

This project is currently in Phase 1, implementing the foundational infrastructure for the COBOL parser.

### Basic Requirements

- The parser should be able to parse COBOL source code and produce an Abstract Syntax Tree (AST)
- The AST should represent the structure of the COBOL code as a tree of nodes starting from the root node which is the program

### Current Features

- ✅ TypeScript project setup with proper configuration
- ✅ CLI interface for parsing COBOL files
- ✅ File I/O operations with error handling
- ✅ Basic logging framework with verbose mode
- ✅ Unit testing framework with Jest
- ✅ Code quality tools (ESLint, Prettier)
- ✅ Basic COBOL file validation

### Requirements

- Node.js 18+ 
- TypeScript 5.0+

### Installation

```bash
npm install
```

### Development

```bash
# Build the project
npm run build

# Run in development mode
npm run dev <cobol-file>

# Run tests
npm test

# Run tests in watch mode
npm run test:watch

# Lint code
npm run lint

# Format code
npm run format
```

### Usage

```bash
# Parse a COBOL file and output JSON to stdout
npm run dev path/to/file.cob

# Parse with verbose output
npm run dev path/to/file.cob --verbose

# Output to a file
npm run dev path/to/file.cob --output result.json

# Output in YAML format
npm run dev path/to/file.cob --format yaml

# Show help
npm run dev --help
```

### CLI Options

- `<file>` - Path to the COBOL file to parse (required)
- `--verbose, -v` - Enable verbose output
- `--format, -f` - Output format: `json` (default) or `yaml`
- `--output, -o` - Output file path (defaults to stdout)
- `--help, -h` - Show help
- `--version, -V` - Show version

### Examples

```bash
# Basic usage
npm run dev test-data-real/cobol/example_1.PCO

# Verbose mode with JSON output to file
npm run dev test-data-real/cobol/example_1.PCO --verbose --output output.json

# YAML output
npm run dev test-data-real/cobol/example_1.PCO --format yaml
```

### Project Structure

```
├── src/
│   ├── cli/           # CLI argument handling
│   ├── parser/        # Parser implementation (future phases)
│   ├── grammar/       # COBOL grammar definitions (future phases)
│   ├── types/         # TypeScript type definitions
│   └── utils/         # Utility functions (logger, file handling)
├── tests/             # Unit tests
├── test-data-real/    # Private test COBOL files (gitignored)
└── plans/             # Implementation plan documentation
```

### Current Output

In Phase 1, the parser validates and reads COBOL files, outputting basic file statistics:

```json
{
  "success": true,
  "file": "path/to/file.cob",
  "message": "File successfully read and validated",
  "stats": {
    "size": 1090346,
    "lines": 26561
  }
}
```

### Development Roadmap

This project follows a phased implementation approach:

- **Phase 1**: ✅ Project Setup and Basic Infrastructure
- **Phase 2**: Basic COBOL Line Processing
- **Phase 3**: Procedure Division Structure Parsing
- **Phase 4**: PERFORM Statement Analysis
- **Phase 5**: Basic AST Construction and JSON Output
- **Phase 6**: COPY Statement Detection and Resolution
- **Phase 7**: Enhanced Multi-File Parsing
- **Phase 8**: Comprehensive Testing and Validation

### Testing

The project includes comprehensive unit tests:

```bash
# Run all tests
npm test

# Run tests in watch mode
npm run test:watch
```

Current test coverage includes:
- Logger functionality
- CLI argument validation
- File I/O operations

### Contributing

1. Follow the existing code style (enforced by ESLint and Prettier)
2. Add tests for new functionality
3. Update documentation as needed
4. All code should pass `npm run lint` and `npm test`

### License

MIT
- Below the root node it there shoul nodes for the divisions. At first we are interested only in the PROCEDURE DIVISION.
- Below the PROCEDURE DIVISION node there should be nodes for the sections and paragraphs.
- Each section and paragraph node must have:
    - A name which is the name of the section or paragraph.
    - The list of source code lines that belong to the section or paragraph.
    - the list of paragraphs or sections that are called by the section or paragraph with the PERFORM statement.

# COPY inclusion requirements
In Cobol the source code can include other source code files using the `COPY` statement. The parser should be able to handle this by:
- Finding the `COPY` statements in the source code. At the moment we are interested only in the `COPY` statements that are in the PROCEDURE DIVISION.
- Reading the content of the included files and replace the `COPY` statement with the content of the included file.
- The inclusion must be recursive, meaning that if the included file has its own `COPY` statements, those must be resolved as well.
- A link between each line of code and the file it comes from must be maintained in the AST. what I suggest is to model each line of code as a node in the AST with a reference to the file it comes from. This way we can keep track of the origin of each line of code in the AST. If you have better ideas, please let me know.

# Technical requirements
- The parser should be implemented in Typescript and should run as a Node.js application
- The parser Node.js application expcts the path to the Cobol source code file as a command line argument.
- The parser should generated an AST in JSON format and save it to a file named as the name of the source code file with a postfix `ast.json` in the same directory as the source code file.    