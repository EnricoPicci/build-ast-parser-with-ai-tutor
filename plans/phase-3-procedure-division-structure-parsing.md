# Phase 3: PROCEDURE DIVISION Structure Parsing

## Overview
This phase implements parsing of COBOL PROCEDURE DIVISION structure to identify sections and paragraphs without COPY statement handling. The focus is on building the basic AST structure for a single file.

## Tasks
1. Identify PROCEDURE DIVISION start in processed source
2. Parse section declarations (NAME SECTION.)
3. Parse paragraph declarations (NAME.)
4. Determine section/paragraph boundaries
5. Associate source lines with sections/paragraphs
6. Build basic AST structure for PROCEDURE DIVISION
7. Handle nested paragraph-section relationships

## Parsing Strategy
- **State Machine Approach**: Track current parsing state (in section, in paragraph, etc.)
- **Pattern Matching**: Use regular expressions to identify section/paragraph headers
- **Boundary Detection**: Determine where sections/paragraphs end based on next structure or division end
- **Line Association**: Group source lines under appropriate structural elements

## COBOL Structure Recognition
**Section Declaration:**
```cobol
SECTION-NAME SECTION.
```

**Paragraph Declaration:**
```cobol
PARAGRAPH-NAME.
```

**Hierarchy Rules:**
- Sections can contain paragraphs
- Paragraphs can exist independently
- Sections end when next section starts or division ends
- Paragraphs end when next paragraph/section starts

## Key Features
- **Structure Identification**: Accurately identify sections and paragraphs
- **Boundary Detection**: Determine precise boundaries of structural elements
- **Line Association**: Associate each code line with its containing structure
- **Hierarchy Building**: Build proper parent-child relationships

## Deliverables
- `ProcedureDivisionParser` class for structure parsing
- Basic AST node definitions for PROCEDURE DIVISION, sections, and paragraphs
- Line association logic for structural elements
- Unit tests for various COBOL structure scenarios
- AST validation logic for structural correctness

## Test Cases
- PROCEDURE DIVISION with sections only
- PROCEDURE DIVISION with paragraphs only
- Mixed sections and paragraphs
- Empty sections/paragraphs
- Nested paragraph structures within sections
- Malformed structure declarations

## Edge Cases to Handle
- Paragraphs within sections
- Multiple consecutive sections
- Empty structural elements
- Comments between structures
- Malformed declarations

## AST Structure (Basic)
```typescript
interface ProcedureDivisionNode {
  type: 'PROCEDURE_DIVISION'
  sections: SectionNode[]
  paragraphs: ParagraphNode[]  // Standalone paragraphs
  sourceLines: string[]
}

interface SectionNode {
  type: 'SECTION'
  name: string
  paragraphs: ParagraphNode[]
  sourceLines: string[]
  startLine: number
  endLine: number
}

interface ParagraphNode {
  type: 'PARAGRAPH'
  name: string
  sourceLines: string[]
  startLine: number
  endLine: number
}
```

## Acceptance Criteria
- Correctly identify all sections and paragraphs in PROCEDURE DIVISION
- Properly associate source lines with structural elements
- Build accurate hierarchical relationships
- Handle various COBOL structure patterns
- Provide clear error messages for malformed structures

## Duration
4-5 days

## Dependencies
- Phase 2: Basic COBOL Line Processing

## Next Phase
Phase 4: PERFORM Statement Analysis will analyze the parsed structure to identify PERFORM relationships.
