# COBOL PROCEDURE DIVISION Parser Implementation Plan

## Project Overview

This document outlines the implementation plan for building a custom TypeScript parser that parses COBOL PROCEDURE DIVISION and generates an Abstract Syntax Tree (AST) in JSON format.

## Requirements Summary

Based on the README.md:
- Parse COBOL source code and produce an AST
- Focus on PROCEDURE DIVISION only (initially)
- Extract sections and paragraphs with names, source lines, and PERFORM relationships
- TypeScript implementation running as Node.js CLI application
- Input: COBOL file path as command line argument
- Output: JSON file with `.ast.json` suffix in same directory

## Architecture Design

### High-Level Components

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   CLI Handler   │───►│   File Reader    │───►│  Line Processor │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                         │
                                                         ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│  JSON Writer    │◄───│   AST Builder    │◄───│  Parser Engine  │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                         │
                                                         ▼
                                               ┌─────────────────┐
                                               │ PERFORM Analyzer│
                                               └─────────────────┘
```

### Core Classes

1. **CobolParser** - Main orchestrator
2. **LineProcessor** - Handle COBOL format specifics
3. **ProcedureDivisionParser** - Parse procedure division structure
4. **PerformAnalyzer** - Extract PERFORM relationships
5. **ASTBuilder** - Construct final AST
6. **JSONFormatter** - Generate output JSON

## Implementation Phases

### Phase 1: Project Setup and Basic Infrastructure

**Tasks:**
1. Initialize TypeScript Node.js project
2. Set up development tooling (ESLint, Prettier, Jest)
3. Create basic project structure
4. Implement CLI argument handling
5. Create basic file I/O operations

**Deliverables:**
- Working CLI that accepts file path
- Basic error handling for file operations
- Project structure with all directories

**Duration:** 1-2 days

### Phase 2: COBOL Line Processing

**Tasks:**
1. Implement COBOL format handling (columns 7-72)
2. Handle comment lines (* and / in column 7)
3. Process continuation lines (- in column 7)
4. Normalize whitespace and case
5. Extract PROCEDURE DIVISION boundaries

**Key Challenges:**
- Fixed-format COBOL with column restrictions
- Variable line lengths and formats
- Continuation line handling

**Test Cases:**
- Lines with sequence numbers
- Comment lines
- Continuation lines
- Mixed case input
- Lines with trailing spaces

**Deliverables:**
- `LineProcessor` class with comprehensive preprocessing
- Unit tests for all line processing scenarios

**Duration:** 2-3 days

### Phase 3: Basic Structure Parsing

**Tasks:**
1. Identify PROCEDURE DIVISION start
2. Parse section declarations (NAME SECTION.)
3. Parse paragraph declarations (NAME.)
4. Determine section/paragraph boundaries
5. Associate source lines with sections/paragraphs

**Parsing Strategy:**
- Line-by-line analysis with state machine
- Pattern matching for section/paragraph headers
- Boundary detection using next section/paragraph or end of division

**Edge Cases:**
- Paragraphs without explicit sections
- Nested sections (if any)
- Empty paragraphs
- Malformed declarations

**Deliverables:**
- `ProcedureDivisionParser` class
- Basic AST structure for sections and paragraphs
- Unit tests with sample COBOL procedures

**Duration:** 3-4 days

### Phase 4: PERFORM Statement Analysis

**Tasks:**
1. Identify all PERFORM statement variants
2. Extract target procedure names
3. Handle PERFORM THROUGH constructs
4. Parse loop-based PERFORM statements
5. Build call relationship mappings

**PERFORM Variants to Handle:**

#### Simple PERFORM
```cobol
PERFORM PARAGRAPH-NAME
PERFORM SECTION-NAME
```

#### PERFORM THROUGH
```cobol
PERFORM PARA-1 THROUGH PARA-5
PERFORM SECTION-1 THROUGH SECTION-3
```

#### Loop PERFORM (Basic Detection)
```cobol
PERFORM PARAGRAPH-NAME 5 TIMES
PERFORM PARAGRAPH-NAME UNTIL condition
PERFORM PARAGRAPH-NAME VARYING variable FROM 1 BY 1 UNTIL condition
```

**Parsing Approach:**
- Regular expression patterns for each variant
- State-based parsing for complex PERFORM statements
- Handle multi-line PERFORM statements

**Deliverables:**
- `PerformAnalyzer` class
- Comprehensive PERFORM statement detection
- Call relationship data structures

**Duration:** 4-5 days

### Phase 5: AST Construction

**Tasks:**
1. Define complete AST node interfaces
2. Build hierarchical AST structure
3. Populate source line arrays
4. Add PERFORM relationship data
5. Implement AST validation

**AST Structure:**
```typescript
interface ProgramNode {
  type: 'PROGRAM'
  name: string
  procedureDivision: ProcedureDivisionNode
  sourceLines: string[]
  children: ASTNode[]
}

interface ProcedureDivisionNode {
  type: 'PROCEDURE_DIVISION'
  sections: SectionNode[]
  paragraphs: ParagraphNode[]
  sourceLines: string[]
  children: ASTNode[]
}

interface SectionNode {
  type: 'SECTION'
  name: string
  paragraphs: ParagraphNode[]
  sourceLines: string[]
  performedBy: string[]
  performs: string[]
  children: ASTNode[]
}

interface ParagraphNode {
  type: 'PARAGRAPH'
  name: string
  sourceLines: string[]
  performedBy: string[]
  performs: string[]
  children: ASTNode[]
}
```

**Deliverables:**
- Complete AST type definitions
- `ASTBuilder` class
- AST validation logic
- Integration tests

**Duration:** 2-3 days

### Phase 6: JSON Output and CLI Integration

**Tasks:**
1. Implement JSON serialization
2. Generate output filename (.ast.json)
3. Handle file writing with error handling
4. Add command-line options and help
5. Implement comprehensive error reporting

**CLI Features:**
- File path validation
- Output directory handling
- Verbose/quiet modes
- Error message formatting

**Deliverables:**
- Complete CLI application
- JSON output formatting
- Error handling and user feedback
- End-to-end integration tests

**Duration:** 2 days

### Phase 7: Testing and Validation

**Tasks:**
1. Create comprehensive test suite
2. Develop test COBOL programs
3. Validate against real-world COBOL code
4. Performance testing
5. Error handling verification

**Test Categories:**
- Unit tests for each component
- Integration tests for complete parsing
- Error handling tests
- Performance benchmarks
- Real COBOL program validation

**Test Data:**
- Simple procedures with basic PERFORM
- Complex procedures with nested calls
- PERFORM THROUGH examples
- Loop-based PERFORM statements
- Malformed COBOL code

**Deliverables:**
- Complete test suite
- Test coverage reports
- Performance benchmarks
- Documentation

**Duration:** 3-4 days

## File Structure

```
src/
├── index.ts                    # CLI entry point
├── cli/
│   ├── cli-handler.ts         # Command line interface
│   └── file-utils.ts          # File I/O utilities
├── parser/
│   ├── cobol-parser.ts        # Main parser orchestrator
│   ├── line-processor.ts      # COBOL line format handling
│   ├── procedure-parser.ts    # PROCEDURE DIVISION parsing
│   └── perform-analyzer.ts    # PERFORM statement analysis
├── ast/
│   ├── ast-builder.ts         # AST construction
│   ├── ast-types.ts           # Type definitions
│   └── ast-validator.ts       # AST validation
├── output/
│   └── json-formatter.ts      # JSON output generation
└── utils/
    ├── cobol-utils.ts         # COBOL-specific utilities
    └── string-utils.ts        # String processing utilities

tests/
├── unit/                      # Unit tests
├── integration/               # Integration tests
├── fixtures/                  # Test COBOL files
└── performance/               # Performance tests

plans/
└── implementation-plan.md     # This document

docs/
├── api-documentation.md       # API documentation
└── usage-examples.md          # Usage examples
```

## Technical Considerations

### COBOL Format Handling

**Fixed Format (Traditional):**
- Columns 1-6: Sequence area (ignored)
- Column 7: Indicator area (*, /, -, space)
- Columns 8-11: Area A (divisions, sections, paragraphs)
- Columns 12-72: Area B (statements, continuations)
- Columns 73-80: Identification area (ignored)

**Free Format (Modern):**
- More flexible column usage
- Different continuation rules
- Mixed format support

### Parsing Challenges

**Case Sensitivity:**
- COBOL is case-insensitive
- Normalize to uppercase for comparison
- Preserve original case in source lines

**Optional Punctuation:**
- Periods after section/paragraph names may be optional
- Handle both WITH and WITHOUT periods

**Name Resolution:**
- Distinguish between section names and paragraph names
- Handle qualified names (section.paragraph)
- Resolve PERFORM targets correctly

### Error Handling Strategy

**Graceful Degradation:**
- Continue parsing after errors when possible
- Mark problematic sections in AST
- Provide detailed error messages with line numbers

**Validation Levels:**
- Syntax validation (structure correctness)
- Semantic validation (name resolution)
- Completeness validation (all PERFORMs resolved)

### Performance Considerations

**Memory Usage:**
- Efficient string handling for large files
- Lazy loading of source lines when possible
- Minimize object creation during parsing

**Processing Speed:**
- Single-pass parsing where possible
- Efficient pattern matching
- Optimized regular expressions

## Success Criteria

1. **Functional Requirements:**
   - Parse PROCEDURE DIVISION structure correctly
   - Extract all sections and paragraphs with names
   - Identify all PERFORM relationships
   - Generate valid JSON AST output

2. **Quality Requirements:**
   - Handle real-world COBOL programs
   - Provide meaningful error messages
   - Process files up to 10,000 lines efficiently
   - 95%+ test coverage

3. **Usability Requirements:**
   - Simple command-line interface
   - Clear documentation and examples
   - Helpful error messages and debugging info

## Risk Mitigation

**Technical Risks:**
- Complex COBOL format variations → Comprehensive test suite
- Performance with large files → Incremental optimization
- Edge cases in PERFORM parsing → Extensive real-world testing

**Project Risks:**
- Scope creep → Strict adherence to initial requirements
- Time overrun → Phased delivery approach
- Quality issues → Test-driven development

## Conclusion

This implementation plan provides a structured approach to building a robust COBOL PROCEDURE DIVISION parser. The phased approach allows for incremental development and testing, while the modular architecture ensures maintainability and extensibility for future enhancements.

Total estimated duration: 17-24 days of development time.
