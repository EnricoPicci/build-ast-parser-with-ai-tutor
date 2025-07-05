# Phase 4: PERFORM Statement Analysis

## Overview
This phase implements detection and analysis of PERFORM statements within the parsed PROCEDURE DIVISION structure. The focus is on identifying call relationships between sections and paragraphs without cross-file complexities.

## Tasks
1. Identify all PERFORM statement variants in source lines
2. Extract target procedure names from PERFORM statements
3. Handle PERFORM THROUGH constructs
4. Parse basic loop-based PERFORM statements
5. Build call relationship mappings between procedures
6. Validate PERFORM targets exist in the current file
7. Create relationship data structures for AST integration

## PERFORM Variants to Handle

### Simple PERFORM
```cobol
PERFORM PARAGRAPH-NAME
PERFORM SECTION-NAME
```

### PERFORM THROUGH
```cobol
PERFORM PARA-1 THROUGH PARA-5
PERFORM SECTION-1 THROUGH SECTION-3
```

### Loop PERFORM (Basic Detection)
```cobol
PERFORM PARAGRAPH-NAME 5 TIMES
PERFORM PARAGRAPH-NAME UNTIL condition
PERFORM PARAGRAPH-NAME VARYING variable FROM 1 BY 1 UNTIL condition
```

## Parsing Approach
- **Regular Expression Patterns**: Define patterns for each PERFORM variant
- **Multi-line Handling**: Handle PERFORM statements spanning multiple lines
- **Target Resolution**: Build symbol table for procedure name lookup
- **Relationship Mapping**: Create bidirectional relationship mappings

## Key Features
- **Comprehensive Detection**: Identify all PERFORM statement types
- **Target Validation**: Verify PERFORM targets exist in parsed structure
- **Relationship Tracking**: Build complete call graphs
- **Error Reporting**: Report unresolved PERFORM targets

## Deliverables
- `PerformAnalyzer` class for PERFORM statement detection
- Target resolution and validation logic
- Call relationship data structures
- Symbol table for procedure name lookup
- Unit tests for all PERFORM variants and edge cases

## Test Cases
- Simple PERFORM statements to paragraphs and sections
- PERFORM THROUGH with various target combinations
- Loop-based PERFORM statements
- Multi-line PERFORM statements
- Invalid PERFORM targets (should be reported as errors)
- Complex nested PERFORM relationships

## Relationship Data Structure
```typescript
interface PerformReference {
  targetName: string
  lineNumber: number
  performType: 'SIMPLE' | 'THROUGH' | 'LOOP'
  throughTarget?: string  // For PERFORM THROUGH
}

interface ProcedureCallGraph {
  procedure: string
  performedBy: PerformReference[]  // Who calls this procedure
  performs: PerformReference[]     // Who this procedure calls
}
```

## Enhanced AST Integration
Update existing AST nodes to include PERFORM relationship information:
```typescript
interface SectionNode {
  // ...existing properties...
  performedBy: PerformReference[]
  performs: PerformReference[]
}

interface ParagraphNode {
  // ...existing properties...
  performedBy: PerformReference[]
  performs: PerformReference[]
}
```

## Error Handling
- Report unresolved PERFORM targets
- Handle malformed PERFORM statements gracefully
- Provide line number context for errors
- Validate THROUGH target ranges

## Acceptance Criteria
- Detect all PERFORM statement variants accurately
- Build complete call relationship mappings
- Validate all PERFORM targets exist
- Integrate relationship data into AST structure
- Provide clear error reporting for invalid references

## Duration
4-5 days

## Dependencies
- Phase 3: PROCEDURE DIVISION Structure Parsing

## Next Phase
Phase 5: Basic AST Construction and JSON Output will integrate all parsed information into the final AST format.
