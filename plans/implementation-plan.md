# COBOL PROCEDURE DIVISION Parser Implementation Plan

## Project Overview

This document outlines the implementation plan for building a custom TypeScript parser that parses COBOL PROCEDURE DIVISION and generates an Abstract Syntax Tree (AST) in JSON format with full COPY statement support.

## Requirements Summary

Based on the README.md:
- Parse COBOL source code and produce an AST
- Focus on PROCEDURE DIVISION only (initially)
- Extract sections and paragraphs with names, source lines, and PERFORM relationships
- Handle COPY statements with recursive inclusion
- Track source file origin for each line of code
- Replace COPY statements with included file content
- TypeScript implementation running as Node.js CLI application
- Input: COBOL file path as command line argument
- Output: JSON file with `.ast.json` suffix in same directory

## Architecture Design

### High-Level Components

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   CLI Handler   │───►│   File Reader    │───►│  COPY Resolver  │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                         │
                                                         ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│  JSON Writer    │◄───│   AST Builder    │◄───│  Line Processor │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                         │
                                                         ▼
                                               ┌─────────────────┐
                                               │  Parser Engine  │
                                               └─────────────────┘
                                                         │
                                                         ▼
                                               ┌─────────────────┐
                                               │ PERFORM Analyzer│
                                               └─────────────────┘
```

### Core Classes

1. **CobolParser** - Main orchestrator
2. **CopyResolver** - Handle COPY statement resolution
3. **LineProcessor** - Handle COBOL format specifics with source tracking
4. **ProcedureDivisionParser** - Parse procedure division structure
5. **PerformAnalyzer** - Extract PERFORM relationships
6. **ASTBuilder** - Construct final AST with source line tracking
7. **JSONFormatter** - Generate output JSON

## Implementation Phases

### Phase 1: Project Setup and Basic Infrastructure

**Tasks:**
1. Initialize TypeScript Node.js project
2. Set up development tooling (ESLint, Prettier, Jest)
3. Create basic project structure
4. Implement CLI argument handling
5. Create basic file I/O operations
6. Design source line tracking model
7. Design data structures for tracking source file origins
8. Plan for recursive file inclusion handling
9. Set up path resolution utilities for COPY statements

**Deliverables:**
- Working CLI that accepts file path
- Basic error handling for file operations
- Project structure with all directories
- Source tracking model design

**Duration:** 2-3 days

### Phase 2: COPY Statement Resolution System

**Tasks:**
1. Implement COPY statement detection
2. Create recursive file inclusion resolver
3. Handle circular dependency detection
4. Implement COPYBOOK search path resolution
5. Create source line tracking during inclusion
6. Handle COPY statement variants

**COPY Statement Variants:**
```cobol
COPY MEMBER-NAME
COPY "filename.cpy"
COPY MEMBER-NAME REPLACING ==OLD== BY ==NEW==
COPY MEMBER-NAME OF LIBRARY-NAME
COPY MEMBER-NAME IN LIBRARY-NAME
```

**Key Features:**
- **Recursive Resolution**: Handle nested COPY statements
- **Source Tracking**: Maintain file origin for each line
- **Circular Detection**: Prevent infinite inclusion loops
- **Path Resolution**: Search standard COPYBOOK directories
- **Error Handling**: Report missing files and circular dependencies

**Data Structures:**
```typescript
interface SourceLine {
  content: string;
  originalLineNumber: number;
  sourceFile: string;
  isFromCopy: boolean;
  copyDepth: number;
  copyPath: string[];
}

interface CopyResolutionContext {
  mainFile: string;
  includedFiles: Set<string>;
  copyStack: string[];
  searchPaths: string[];
}
```

**Deliverables:**
- `CopyResolver` class with full COPY handling
- Source line tracking implementation
- Circular dependency detection
- Unit tests for all COPY variants

**Duration:** 4-5 days

### Phase 3: Enhanced Line Processing with Source Tracking

### Phase 3: Enhanced Line Processing with Source Tracking

**Tasks:**
1. Implement COBOL format handling (columns 7-72) with source tracking
2. Handle comment lines (* and / in column 7) while preserving origin
3. Process continuation lines (- in column 7) across file boundaries
4. Normalize whitespace and case while maintaining source references
5. Extract PROCEDURE DIVISION boundaries from resolved source

**Enhanced Features:**
- **Source Preservation**: Each processed line retains file origin information
- **Cross-File Continuations**: Handle continuation lines that span COPY boundaries
- **Comment Tracking**: Preserve comments with source file references
- **Line Number Mapping**: Maintain mapping between processed and original lines

**Key Challenges:**
- Fixed-format COBOL with column restrictions across multiple files
- Variable line lengths and formats from different sources
- Continuation line handling across COPY boundaries
- Maintaining accurate line number references

**Test Cases:**
- COPY files with different formatting styles
- Continuation lines spanning COPY boundaries
- Comments in both main file and COPY files
- Mixed case input from multiple sources
- Lines with trailing spaces from various files

**Deliverables:**
- Enhanced `LineProcessor` class with source tracking
- Cross-file continuation handling
- Unit tests for all line processing scenarios with COPY files

**Duration:** 3-4 days

### Phase 4: Basic Structure Parsing with Source References

**Tasks:**
1. Identify PROCEDURE DIVISION start across resolved files
2. Parse section declarations (NAME SECTION.) with source tracking
3. Parse paragraph declarations (NAME.) with source tracking
4. Determine section/paragraph boundaries across file boundaries
5. Associate source lines with sections/paragraphs including file origins

**Enhanced Parsing Features:**
- **Cross-File Structures**: Handle sections/paragraphs spanning COPY boundaries
- **Source Attribution**: Track which file each structure element comes from
- **Boundary Detection**: Accurately determine boundaries across included files
- **Origin Mapping**: Maintain clear mapping of AST elements to source files

**Parsing Strategy:**
- Line-by-line analysis with state machine and source tracking
- Pattern matching for section/paragraph headers with file context
- Boundary detection using next section/paragraph or end of division across files

**Edge Cases:**
- Paragraphs defined in COPY files
- Sections starting in main file and continuing in COPY file
- Empty paragraphs in included files
- Malformed declarations across file boundaries

**Deliverables:**
- Enhanced `ProcedureDivisionParser` class with source tracking
- AST structure for sections and paragraphs with file references
- Unit tests with multi-file COBOL procedures

**Duration:** 4-5 days

### Phase 5: PERFORM Statement Analysis with Cross-File Support

### Phase 5: PERFORM Statement Analysis with Cross-File Support

**Tasks:**
1. Identify all PERFORM statement variants across all included files
2. Extract target procedure names with source file tracking
3. Handle PERFORM THROUGH constructs spanning files
4. Parse loop-based PERFORM statements from any source
5. Build call relationship mappings with file context
6. Resolve PERFORM targets across main file and COPY files

**PERFORM Variants to Handle:**

#### Simple PERFORM (with cross-file targets)
```cobol
PERFORM PARAGRAPH-NAME          # May be in COPY file
PERFORM SECTION-NAME           # May be in COPY file
```

#### PERFORM THROUGH (cross-file support)
```cobol
PERFORM PARA-1 THROUGH PARA-5   # PARA-1 in main, PARA-5 in COPY
PERFORM SECTION-1 THROUGH SECTION-3  # Both may be in COPY files
```

#### Loop PERFORM (Basic Detection)
```cobol
PERFORM PARAGRAPH-NAME 5 TIMES      # Target may be in COPY file
PERFORM PARAGRAPH-NAME UNTIL condition
PERFORM PARAGRAPH-NAME VARYING variable FROM 1 BY 1 UNTIL condition
```

**Enhanced Features:**
- **Cross-File Resolution**: Resolve PERFORM targets in any included file
- **Source Context**: Track which file contains each PERFORM statement
- **Target Attribution**: Record source file of PERFORM targets
- **Relationship Mapping**: Build comprehensive call graphs across files

**Parsing Approach:**
- Regular expression patterns for each variant with file context
- State-based parsing for complex PERFORM statements across files
- Handle multi-line PERFORM statements spanning file boundaries
- Build comprehensive symbol table for cross-file name resolution

**Deliverables:**
- Enhanced `PerformAnalyzer` class with cross-file support
- Comprehensive PERFORM statement detection across files
- Call relationship data structures with source attribution
- Symbol resolution across file boundaries

**Duration:** 5-6 days

### Phase 6: Enhanced AST Construction with Source Line Tracking

### Phase 6: Enhanced AST Construction with Source Line Tracking

**Tasks:**
1. Define complete AST node interfaces with source tracking
2. Build hierarchical AST structure with file attribution
3. Populate source line arrays with file origin information
4. Add PERFORM relationship data with cross-file references
5. Implement AST validation with source verification
6. Create source line nodes as suggested in requirements

**Enhanced AST Structure:**
```typescript
interface SourceLineNode extends ASTNode {
  type: 'SOURCE_LINE'
  content: string
  originalLineNumber: number
  sourceFile: string
  isFromCopy: boolean
  copyDepth: number
  copyPath: string[]
}

interface ProgramNode {
  type: 'PROGRAM'
  name: string
  procedureDivision: ProcedureDivisionNode
  sourceLines: SourceLineNode[]  # Array of source line nodes
  children: ASTNode[]
  sourceFiles: string[]  # List of all files involved
}

interface ProcedureDivisionNode {
  type: 'PROCEDURE_DIVISION'
  sections: SectionNode[]
  paragraphs: ParagraphNode[]
  sourceLines: SourceLineNode[]  # With file attribution
  children: ASTNode[]
}

interface SectionNode {
  type: 'SECTION'
  name: string
  paragraphs: ParagraphNode[]
  sourceLines: SourceLineNode[]  # Lines with source file info
  performedBy: PerformReference[]  # With source file context
  performs: PerformReference[]     # With target file context
  children: ASTNode[]
  sourceFile: string  # File where section is defined
}

interface ParagraphNode {
  type: 'PARAGRAPH'
  name: string
  sourceLines: SourceLineNode[]   # Lines with source file info
  performedBy: PerformReference[] # With source file context
  performs: PerformReference[]    # With target file context
  children: ASTNode[]
  sourceFile: string  # File where paragraph is defined
}

interface PerformReference {
  targetName: string
  sourceFile: string      # File containing the PERFORM
  targetFile: string      # File containing the target
  lineNumber: number
  performType: string
}
```

**Key Features:**
- **Source Line Nodes**: Each line becomes a node with file attribution
- **File Tracking**: Every AST element knows its source file
- **Cross-File References**: PERFORM relationships track both source and target files
- **Copy Hierarchy**: Track the depth and path of COPY inclusions

**Deliverables:**
- Complete AST type definitions with source tracking
- Enhanced `ASTBuilder` class with file attribution
- AST validation logic with source verification
- Integration tests with multi-file scenarios

**Duration:** 3-4 days

### Phase 7: JSON Output and CLI Integration

### Phase 7: JSON Output and CLI Integration

**Tasks:**
1. Implement JSON serialization with source tracking information
2. Generate output filename (.ast.json)
3. Handle file writing with error handling
4. Add command-line options for COPYBOOK paths and help
5. Implement comprehensive error reporting with file context
6. Add verbose mode showing file inclusion details

**Enhanced CLI Features:**
- File path validation and COPYBOOK path configuration
- Output directory handling
- Verbose/quiet modes with COPY resolution details
- Error message formatting with file and line context
- COPYBOOK search path configuration
- Circular dependency reporting

**JSON Output Features:**
- Complete source file attribution for every element
- File inclusion hierarchy and statistics
- COPY resolution details and paths
- Cross-file reference mappings

**Deliverables:**
- Complete CLI application with COPY support
- Enhanced JSON output formatting with source tracking
- Error handling and user feedback with file context
- End-to-end integration tests with multi-file scenarios

**Duration:** 3 days

### Phase 8: Testing and Validation with Multi-File Scenarios

### Phase 8: Testing and Validation with Multi-File Scenarios

**Tasks:**
1. Create comprehensive test suite with COPY scenarios
2. Develop test COBOL programs with complex COPY structures
3. Validate against real-world COBOL code with COPYBOOKS
4. Performance testing with large files and deep COPY nesting
5. Error handling verification for COPY-related issues
6. Cross-file reference validation

**Enhanced Test Categories:**
- Unit tests for each component including COPY resolution
- Integration tests for complete parsing with multiple files
- COPY-specific error handling tests (missing files, circular dependencies)
- Performance benchmarks with various COPY scenarios
- Real COBOL program validation with COPYBOOK libraries

**Complex Test Scenarios:**
- **Deep Nesting**: COPY files that include other COPY files (3-4 levels deep)
- **Circular Dependencies**: COPY files that reference each other
- **Cross-File PERFORM**: Main file performs paragraphs in COPY files
- **Mixed Structures**: Sections in main file, paragraphs in COPY files
- **Large Scale**: Programs with 50+ COPY files
- **Path Resolution**: COPY files in different directories

**Test Data Structure:**
```
tests/fixtures/
├── simple/
│   ├── main.cbl
│   └── copybooks/
│       ├── common.cpy
│       └── utils.cpy
├── complex/
│   ├── main.cbl
│   └── copybooks/
│       ├── level1.cpy
│       ├── level2.cpy
│       └── nested/
│           └── level3.cpy
├── circular/
│   ├── main.cbl
│   └── copybooks/
│       ├── circular1.cpy
│       └── circular2.cpy
└── performance/
    ├── large-main.cbl
    └── copybooks/
        ├── copy01.cpy
        ├── copy02.cpy
        └── ... (50+ files)
```

**Deliverables:**
- Comprehensive test suite with COPY scenarios
- Test coverage reports for all components
- Performance benchmarks with COPY resolution
- Documentation for multi-file testing

**Duration:** 4-5 days

## File Structure

```
src/
├── index.ts                    # CLI entry point
├── cli/
│   ├── cli-handler.ts         # Command line interface
│   └── file-utils.ts          # File I/O utilities
├── copy/
│   ├── copy-resolver.ts       # COPY statement resolution
│   ├── copy-detector.ts       # COPY statement detection
│   ├── path-resolver.ts       # COPYBOOK path resolution
│   └── circular-detector.ts   # Circular dependency detection
├── parser/
│   ├── cobol-parser.ts        # Main parser orchestrator
│   ├── line-processor.ts      # COBOL line format handling with source tracking
│   ├── procedure-parser.ts    # PROCEDURE DIVISION parsing
│   └── perform-analyzer.ts    # PERFORM statement analysis with cross-file support
├── ast/
│   ├── ast-builder.ts         # AST construction with source tracking
│   ├── ast-types.ts           # Type definitions with source line nodes
│   ├── ast-validator.ts       # AST validation with source verification
│   └── source-tracker.ts     # Source line and file tracking utilities
├── output/
│   └── json-formatter.ts      # JSON output generation with source info
└── utils/
    ├── cobol-utils.ts         # COBOL-specific utilities
    └── string-utils.ts        # String processing utilities

tests/
├── unit/                      # Unit tests
│   ├── copy-resolver.test.ts  # COPY resolution tests
│   ├── source-tracking.test.ts # Source tracking tests
│   └── cross-file.test.ts     # Cross-file functionality tests
├── integration/               # Integration tests
│   ├── multi-file.test.ts     # Multi-file parsing tests
│   └── copy-scenarios.test.ts # Complex COPY scenarios
├── fixtures/                  # Test COBOL files
│   ├── simple/               # Simple COPY scenarios
│   ├── complex/              # Complex nested COPY scenarios
│   ├── circular/             # Circular dependency test cases
│   └── performance/          # Large-scale test files
└── performance/               # Performance tests
    └── copy-performance.test.ts

plans/
└── implementation-plan.md     # This document

docs/
├── api-documentation.md       # API documentation
├── copy-handling.md          # COPY statement handling documentation
└── usage-examples.md         # Usage examples with multi-file scenarios
```

## Technical Considerations

### COBOL Format Handling with Multi-File Support

**Fixed Format (Traditional):**
- Columns 1-6: Sequence area (ignored, but track source file)
- Column 7: Indicator area (*, /, -, space) - handle across file boundaries
- Columns 8-11: Area A (divisions, sections, paragraphs) - track source file
- Columns 12-72: Area B (statements, continuations) - handle cross-file continuations
- Columns 73-80: Identification area (ignored)

**Free Format (Modern):**
- More flexible column usage across files
- Different continuation rules in COPY files
- Mixed format support between main file and COPY files

### COPY Statement Handling Challenges

**Path Resolution:**
- Standard COPYBOOK library paths
- Relative paths from including file
- Environment variable resolution
- Case-sensitive vs case-insensitive file systems

**Recursive Processing:**
- Maintain inclusion stack to prevent infinite loops
- Track inclusion depth for debugging
- Handle COPY statements within COPY files
- Preserve source context through all levels

**Replacement Handling:**
```cobol
COPY MEMBER REPLACING ==:PREFIX:== BY ==WS==
COPY MEMBER REPLACING ==OLD-VALUE== BY ==NEW-VALUE==
                      ==ANOTHER-OLD== BY ==ANOTHER-NEW==
```

**Error Scenarios:**
- Missing COPY files
- Circular dependencies
- Path resolution failures
- Malformed COPY statements
- Permission issues reading COPY files

### Enhanced Parsing Challenges

**Case Sensitivity:**
- COBOL is case-insensitive across all files
- Normalize to uppercase for comparison
- Preserve original case in source lines with file attribution

**Cross-File Name Resolution:**
- Build comprehensive symbol table across all files
- Handle name collisions between main file and COPY files
- Resolve PERFORM targets in any included file
- Maintain scoping rules across file boundaries

**Source Line Attribution:**
- Every line must track its original file and line number
- Continuation lines may span file boundaries
- Comments must preserve file context
- Error reporting must reference original source locations

### Performance Considerations with COPY Files

**Memory Usage:**
- Efficient string handling for large files with many COPY inclusions
- Lazy loading of COPY files when possible
- Minimize object creation during recursive parsing
- Cache resolved COPY files to avoid re-reading

**Processing Speed:**
- Single-pass parsing where possible across all files
- Efficient pattern matching for COPY statements
- Optimized regular expressions for multi-file scenarios
- Parallel processing of independent COPY files where safe

**Circular Dependency Detection:**
- Fast cycle detection algorithms
- Minimal overhead for tracking inclusion stack
- Early termination on circular detection

### Enhanced Error Handling Strategy

**Multi-File Error Context:**
- Report errors with full file path and line number context
- Show inclusion chain for errors in COPY files
- Provide suggestions for missing COPY files
- Clear reporting of circular dependency chains

**Graceful Degradation:**
- Continue parsing after COPY resolution failures when possible
- Mark missing sections in AST with source attribution
- Provide detailed error messages with file inclusion context
- Maintain partial AST with available information

**Validation Levels:**
- COPY resolution validation (all files found and readable)
- Syntax validation (structure correctness across files)
- Semantic validation (cross-file name resolution)
- Completeness validation (all PERFORMs resolved across files)

## Success Criteria

1. **Functional Requirements:**
   - Parse PROCEDURE DIVISION structure correctly across multiple files
   - Extract all sections and paragraphs with names and source file attribution
   - Identify all PERFORM relationships including cross-file references
   - Generate valid JSON AST output with complete source tracking
   - **Resolve all COPY statements recursively with proper error handling**
   - **Maintain accurate source line attribution for every AST element**
   - **Handle circular COPY dependencies gracefully**

2. **Quality Requirements:**
   - Handle real-world COBOL programs with complex COPYBOOK structures
   - Provide meaningful error messages with full file context
   - Process files up to 10,000 lines with 50+ COPY files efficiently
   - 95%+ test coverage including multi-file scenarios
   - **Detect and report circular dependencies clearly**
   - **Resolve COPY files from standard library paths**

3. **Usability Requirements:**
   - Simple command-line interface with COPYBOOK path configuration
   - Clear documentation and examples for multi-file scenarios
   - Helpful error messages with file inclusion context and debugging info
   - **Support for standard COBOL COPYBOOK directory conventions**

## Risk Mitigation

**Technical Risks:**
- Complex COBOL format variations across multiple files → Comprehensive test suite with diverse COPYBOOK formats
- Performance with large files and deep COPY nesting → Incremental optimization and caching strategies
- Edge cases in PERFORM parsing across files → Extensive real-world testing with complex COPYBOOK scenarios
- **Circular dependency complexity** → Robust cycle detection algorithms and clear error reporting
- **Path resolution across platforms** → Cross-platform testing and standardized path handling
- **Memory usage with large COPYBOOK hierarchies** → Memory profiling and optimization

**Project Risks:**
- Scope creep beyond COPY requirements → Strict adherence to initial requirements with COPY support
- Time overrun due to COPY complexity → Phased delivery approach with COPY as separate phase
- Quality issues with multi-file scenarios → Test-driven development with extensive COPY test cases
- **COPYBOOK availability for testing** → Create comprehensive synthetic test COPYBOOK libraries

## Conclusion

This enhanced implementation plan provides a structured approach to building a robust COBOL PROCEDURE DIVISION parser with full COPY statement support. The addition of COPY handling significantly increases complexity but is essential for real-world COBOL parsing.

Key additions include:
- **Recursive COPY resolution** with circular dependency detection
- **Source line tracking** as AST nodes with file attribution
- **Cross-file PERFORM analysis** and name resolution
- **Enhanced error handling** with multi-file context
- **Comprehensive testing** with complex COPYBOOK scenarios

The phased approach allows for incremental development of COPY functionality while maintaining the core parsing capabilities. The modular architecture ensures maintainability and provides a solid foundation for future enhancements.

**Total estimated duration: 26-32 days of development time** (increased from 17-24 days due to COPY complexity).

The source line tracking model using AST nodes for each line provides the flexibility requested in the requirements while maintaining clear file attribution throughout the parsing process.
