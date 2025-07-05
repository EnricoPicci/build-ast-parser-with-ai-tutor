# Phase 2: Basic COBOL Line Processing

## Overview
This phase implements fundamental COBOL file processing capabilities without COPY statement handling. The focus is on understanding COBOL format conventions and preparing lines for parsing.

## Tasks
1. Implement COBOL format handling (columns 7-72 for fixed format)
2. Handle comment lines detection (* and / in column 7)
3. Process continuation lines (- in column 7)
4. Normalize whitespace and case handling for COBOL
5. Extract PROCEDURE DIVISION boundaries from source
6. Create basic line filtering and preprocessing
7. Implement COBOL-specific utilities for format detection

## Key Features
- **Fixed Format Support**: Proper handling of COBOL column restrictions
- **Comment Recognition**: Identify and handle comment lines appropriately
- **Line Continuation**: Process continuation lines according to COBOL rules
- **Case Normalization**: Handle COBOL's case-insensitive nature
- **Format Detection**: Distinguish between fixed and free format COBOL

## COBOL Format Handling
**Fixed Format (Traditional):**
- Columns 1-6: Sequence area (ignored)
- Column 7: Indicator area (*, /, -, space)
- Columns 8-11: Area A (divisions, sections, paragraphs)
- Columns 12-72: Area B (statements, continuations)
- Columns 73-80: Identification area (ignored)

**Free Format (Modern):**
- More flexible column usage
- Different continuation rules
- Support for both formats

## Deliverables
- `LineProcessor` class for COBOL format handling
- COBOL format detection and processing
- Line preprocessing with comment and continuation handling
- Unit tests for various COBOL format scenarios
- Utility functions for COBOL-specific string processing

## Test Cases
- Fixed format COBOL with proper column usage
- Free format COBOL files
- Mixed comment and code lines
- Continuation lines spanning multiple lines
- Various whitespace and case combinations
- Malformed line formats

## Technical Considerations
- Handle variable line lengths gracefully
- Preserve original line information for error reporting
- Efficient string processing for large files
- Support for both Windows and Unix line endings

## Acceptance Criteria
- Correctly process fixed format COBOL files
- Handle comment lines without breaking parsing
- Process continuation lines properly
- Extract clean, normalized lines for further processing
- Maintain line number references for error reporting

## Duration
3-4 days

## Dependencies
- Phase 1: Project Setup and Basic Infrastructure

## Next Phase
Phase 3: PROCEDURE DIVISION Structure Parsing will use the processed lines to identify sections and paragraphs.
