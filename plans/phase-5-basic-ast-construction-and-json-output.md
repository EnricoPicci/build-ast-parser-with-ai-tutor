# Phase 5: Basic AST Construction and JSON Output

## Overview
This phase integrates all parsing components to build the complete AST structure and generate JSON output. This creates a working parser for single COBOL files without COPY statement support.

## Tasks
1. Define complete AST node interfaces
2. Build hierarchical AST structure from parsed components
3. Integrate PERFORM relationship data into AST
4. Implement AST validation logic
5. Create JSON serialization for AST output
6. Generate output filename (.ast.json)
7. Implement end-to-end integration testing

## Complete AST Structure
```typescript
interface ASTNode {
  type: string
  children: ASTNode[]
}

interface ProgramNode extends ASTNode {
  type: 'PROGRAM'
  name: string
  procedureDivision: ProcedureDivisionNode
  children: ASTNode[]
}

interface ProcedureDivisionNode extends ASTNode {
  type: 'PROCEDURE_DIVISION'
  sections: SectionNode[]
  paragraphs: ParagraphNode[]  // Standalone paragraphs
  sourceLines: string[]
  children: ASTNode[]
}

interface SectionNode extends ASTNode {
  type: 'SECTION'
  name: string
  paragraphs: ParagraphNode[]
  sourceLines: string[]
  performedBy: PerformReference[]
  performs: PerformReference[]
  children: ASTNode[]
}

interface ParagraphNode extends ASTNode {
  type: 'PARAGRAPH'
  name: string
  sourceLines: string[]
  performedBy: PerformReference[]
  performs: PerformReference[]
  children: ASTNode[]
}

interface PerformReference {
  targetName: string
  lineNumber: number
  performType: 'SIMPLE' | 'THROUGH' | 'LOOP'
  throughTarget?: string
}
```

## Key Features
- **Hierarchical Structure**: Complete tree representation of COBOL program
- **Relationship Integration**: PERFORM relationships embedded in AST
- **Validation**: Ensure AST structural correctness
- **JSON Serialization**: Clean, readable JSON output
- **Error Handling**: Graceful handling of parsing issues

## AST Construction Process
1. **Initialize Root**: Create program node with extracted program name
2. **Build Structure**: Construct PROCEDURE DIVISION with sections/paragraphs
3. **Associate Lines**: Populate source lines for each structural element
4. **Add Relationships**: Integrate PERFORM analysis results
5. **Validate**: Check AST completeness and consistency
6. **Serialize**: Convert to JSON format for output

## Deliverables
- `ASTBuilder` class for complete AST construction
- AST validation logic with comprehensive checks
- JSON serialization with proper formatting
- Output file generation with .ast.json naming
- End-to-end integration tests
- Complete CLI integration

## JSON Output Features
- Pretty-printed JSON for readability
- Consistent property ordering
- Proper handling of optional fields
- Error information in output when applicable

## Validation Rules
- All PERFORM targets must resolve to existing procedures
- Section/paragraph names must be unique within scope
- Source lines must be properly associated
- AST structure must be valid hierarchy

## CLI Integration
- Complete command-line processing
- File path validation
- Output file generation in same directory as input
- Error reporting and user feedback
- Help and usage information

## Test Scenarios
- Simple COBOL programs with basic structure
- Complex programs with multiple sections and paragraphs
- Programs with various PERFORM patterns
- Error cases (missing targets, malformed code)
- Large programs for performance validation

## Error Handling Strategy
- Collect all parsing errors and warnings
- Continue parsing where possible to provide partial results
- Include error information in JSON output
- Provide meaningful error messages with line numbers

## Acceptance Criteria
- Generate complete, valid AST for well-formed COBOL programs
- Produce readable JSON output with .ast.json extension
- Handle parsing errors gracefully with meaningful messages
- Process real COBOL files successfully
- Meet performance requirements for typical file sizes

## Duration
3-4 days

## Dependencies
- Phase 4: PERFORM Statement Analysis

## Next Phase
Phase 6: COPY Statement Detection and Resolution will extend this foundation to handle multi-file COBOL programs.
