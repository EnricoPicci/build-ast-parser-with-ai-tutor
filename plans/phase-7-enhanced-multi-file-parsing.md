# Phase 7: Enhanced Multi-File Parsing

## Overview
This phase integrates COPY resolution with the existing parsing pipeline to handle cross-file structures, relationships, and source line tracking throughout the entire AST.

## Tasks
1. Integrate COPY resolution with line processing
2. Handle cross-file structure parsing (sections/paragraphs in COPY files)
3. Implement cross-file PERFORM analysis and resolution
4. Update AST construction to include source line nodes
5. Handle continuation lines spanning file boundaries
6. Implement comprehensive symbol resolution across files
7. Update JSON output to include multi-file information

## Cross-File Parsing Challenges
- **Structure Boundaries**: Sections/paragraphs may span COPY boundaries
- **Name Resolution**: PERFORM targets may be in different files
- **Source Attribution**: Every AST element must know its source file
- **Continuation Lines**: Handle line continuations across file boundaries
- **Error Context**: Error reporting must include file inclusion context

## Enhanced Line Processing
- Process source-tracked lines from COPY resolution
- Handle continuation lines that may span file boundaries
- Maintain accurate line number mapping across files
- Preserve comment context with file attribution

## Cross-File Structure Parsing
- Parse sections and paragraphs regardless of source file
- Track which file each structure element comes from
- Handle structures that span multiple files
- Maintain proper hierarchical relationships across file boundaries

## Enhanced PERFORM Analysis
- Resolve PERFORM targets across all included files
- Build comprehensive symbol table from all files
- Track source and target files for each PERFORM relationship
- Report cross-file call relationships in AST

## Source Line Nodes Implementation
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

// Enhanced AST nodes with source line tracking
interface ProgramNode extends ASTNode {
  type: 'PROGRAM'
  name: string
  procedureDivision: ProcedureDivisionNode
  sourceLines: SourceLineNode[]  // All source lines as nodes
  sourceFiles: string[]          // List of all involved files
  children: ASTNode[]
}

interface SectionNode extends ASTNode {
  type: 'SECTION'
  name: string
  paragraphs: ParagraphNode[]
  sourceLines: SourceLineNode[]  // Lines with file attribution
  performedBy: PerformReference[] // With cross-file context
  performs: PerformReference[]    // With cross-file context
  sourceFile: string             // File where section is defined
  children: ASTNode[]
}

interface PerformReference {
  targetName: string
  sourceFile: string    // File containing the PERFORM
  targetFile: string    // File containing the target
  lineNumber: number
  performType: string
}
```

## Key Features
- **Source Line Nodes**: Each line becomes an AST node with file attribution
- **Cross-File Resolution**: Handle structures and relationships spanning files
- **Complete Attribution**: Every element knows its source file
- **Enhanced Relationships**: PERFORM relationships track both source and target files

## Integration Process
1. **Enhanced Line Processing**: Use COPY-resolved lines with source tracking
2. **Multi-File Parsing**: Parse structures from all included files
3. **Symbol Table Building**: Create comprehensive cross-file symbol table
4. **Relationship Resolution**: Resolve PERFORM targets across all files
5. **AST Construction**: Build AST with source line nodes and file attribution
6. **Validation**: Ensure all cross-file references are valid

## Deliverables
- Enhanced `LineProcessor` with COPY integration
- Updated `ProcedureDivisionParser` for multi-file support
- Enhanced `PerformAnalyzer` with cross-file resolution
- Updated `ASTBuilder` with source line node support
- Comprehensive symbol table implementation
- Integration tests for multi-file scenarios

## Test Scenarios
- **Deep Nesting**: COPY files including other COPY files
- **Cross-File PERFORM**: Main file performs paragraphs in COPY files
- **Mixed Structures**: Sections in main file, paragraphs in COPY files
- **Complex Relationships**: Multiple levels of cross-file calls
- **Boundary Spanning**: Structures that cross file boundaries
- **Error Cases**: Missing targets across files

## Error Handling Enhancements
- Report errors with full file context and inclusion chain
- Show which file contains unresolved PERFORM targets
- Provide meaningful error messages for cross-file issues
- Include source file information in all error reports

## Validation Requirements
- All PERFORM targets must resolve across all files
- Source file attribution must be complete and accurate
- AST structure must be valid with proper file tracking
- No broken cross-file references

## Performance Considerations
- Efficient symbol table lookups across multiple files
- Minimize memory usage with large COPYBOOK hierarchies
- Optimize source line node creation and management
- Cache resolved symbols to avoid redundant lookups

## Acceptance Criteria
- Successfully parse COBOL programs with complex COPYBOOK structures
- Resolve all PERFORM relationships across files
- Generate AST with complete source line tracking
- Handle real-world COBOL programs with deep COPY nesting
- Provide accurate cross-file error reporting

## Duration
4-5 days

## Dependencies
- Phase 6: COPY Statement Detection and Resolution

## Next Phase
Phase 8: Comprehensive Testing and Validation will create thorough test suites for all multi-file scenarios and edge cases.
