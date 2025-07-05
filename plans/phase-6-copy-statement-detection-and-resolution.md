# Phase 6: COPY Statement Detection and Resolution

## Overview
This phase extends the parser to handle COPY statements, implementing recursive file inclusion with source tracking. This is where the parser becomes capable of handling real-world COBOL programs with COPYBOOK dependencies.

## Tasks
1. Implement COPY statement detection in PROCEDURE DIVISION
2. Create recursive file inclusion resolver
3. Implement COPYBOOK search path resolution
4. Handle circular dependency detection
5. Design and implement source line tracking model
6. Create source file attribution system
7. Handle COPY statement variants and replacement

## COPY Statement Variants
```cobol
COPY MEMBER-NAME
COPY "filename.cpy"
COPY MEMBER-NAME REPLACING ==OLD== BY ==NEW==
COPY MEMBER-NAME OF LIBRARY-NAME
COPY MEMBER-NAME IN LIBRARY-NAME
```

## Key Features
- **Recursive Resolution**: Handle nested COPY statements in included files
- **Source Tracking**: Maintain file origin for every line of code
- **Circular Detection**: Prevent infinite inclusion loops
- **Path Resolution**: Search standard COPYBOOK directories
- **Replacement Handling**: Process COPY REPLACING clauses
- **Error Handling**: Report missing files and circular dependencies

## Source Tracking Model
```typescript
interface SourceLine {
  content: string
  originalLineNumber: number
  sourceFile: string
  isFromCopy: boolean
  copyDepth: number
  copyPath: string[]
}

interface CopyResolutionContext {
  mainFile: string
  includedFiles: Set<string>
  copyStack: string[]
  searchPaths: string[]
}
```

## Resolution Process
1. **Scan for COPY**: Identify COPY statements in source lines
2. **Resolve Paths**: Find COPYBOOK files using search paths
3. **Check Cycles**: Detect circular dependencies before inclusion
4. **Read Content**: Load COPYBOOK content with error handling
5. **Apply Replacements**: Process COPY REPLACING clauses if present
6. **Track Source**: Maintain file origin information for each line
7. **Recurse**: Process COPY statements in included files

## Path Resolution Strategy
- Current directory relative to including file
- Standard COPYBOOK library directories
- Environment variable paths (COBCOPY, etc.)
- Configurable search paths via CLI options

## Circular Dependency Detection
- Maintain inclusion stack during resolution
- Check each file against current inclusion path
- Report complete dependency chain in error messages
- Fail fast to prevent infinite loops

## Deliverables
- `CopyResolver` class with full COPY handling
- `CopyDetector` for COPY statement identification
- `PathResolver` for COPYBOOK file location
- `CircularDetector` for dependency cycle detection
- Source line tracking implementation
- Unit tests for all COPY scenarios

## Test Cases
- Simple COPY inclusions
- Nested COPY statements (3-4 levels deep)
- Circular dependency scenarios
- Missing COPYBOOK files
- COPY REPLACING variations
- Path resolution in different directories
- Mixed case file names and paths

## Error Scenarios
- Missing COPY files with helpful error messages
- Circular dependencies with full chain reporting
- Path resolution failures
- Malformed COPY statements
- Permission issues reading COPY files
- Invalid replacement patterns

## Integration with Existing Parser
- Extend LineProcessor to handle source-tracked lines
- Update ProcedureDivisionParser for multi-file structures
- Enhance PerformAnalyzer for cross-file PERFORM resolution
- Modify ASTBuilder to include source file information

## Enhanced Data Structures
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

// Update existing nodes to include source tracking
interface SectionNode extends ASTNode {
  // ...existing properties...
  sourceFile: string
  sourceLines: SourceLineNode[]
}
```

## Acceptance Criteria
- Successfully resolve all COPY statements recursively
- Detect and report circular dependencies clearly
- Maintain accurate source file attribution for every line
- Handle all common COPY statement variants
- Provide meaningful error messages for COPY-related issues
- Process real-world COBOL programs with complex COPYBOOK structures

## Duration
5-6 days

## Dependencies
- Phase 5: Basic AST Construction and JSON Output

## Next Phase
Phase 7: Enhanced Multi-File Parsing will integrate COPY resolution with the existing parsing pipeline to handle cross-file structures and relationships.
