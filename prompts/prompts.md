# PROMPT 1
Add the content of the README.md file and generate your best recommendations on how to design a solution to the requirements defined in README.md

# Response 1
EnricoPicci: Add the content of the README.md file and generate your best recommendations on how to design a solution to the requirements defined in README.md

GitHub Copilot: I can see the README.md content from the attachment. Let me analyze the requirements and provide my best recommendations for designing a COBOL AST parser solution.

## Analysis of Requirements

Based on the README.md, you need to build a COBOL AST parser with these key features:

### Functional Requirements
- Parse COBOL source code into an Abstract Syntax Tree
- Focus initially on the PROCEDURE DIVISION
- Extract sections and paragraphs with their names, source lines, and PERFORM call relationships
- Output AST as JSON

### Technical Requirements
- TypeScript implementation
- Node.js CLI application
- Input: COBOL file path as command line argument
- Output: JSON file with `.ast.json` suffix

## Design Recommendations

### 1. Project Structure
```
src/
├── index.ts              # CLI entry point
├── parser/
│   ├── lexer.ts         # Tokenization
│   ├── parser.ts        # Main parser logic
│   └── ast-nodes.ts     # AST node definitions
├── analyzer/
│   └── perform-analyzer.ts  # PERFORM statement analysis
├── output/
│   └── json-formatter.ts    # JSON output formatting
└── types/
    └── cobol-types.ts       # Type definitions

tests/
├── fixtures/            # Sample COBOL files
└── parser.test.ts       # Unit tests

package.json
tsconfig.json
README.md
```

### 2. AST Node Structure
```typescript
interface ASTNode {
  type: string;
  name?: string;
  sourceLines: number[];
  children: ASTNode[];
}

interface ProgramNode extends ASTNode {
  type: 'PROGRAM';
  divisions: DivisionNode[];
}

interface ProcedureDivisionNode extends ASTNode {
  type: 'PROCEDURE_DIVISION';
  sections: SectionNode[];
  paragraphs: ParagraphNode[];
}

interface SectionNode extends ASTNode {
  type: 'SECTION';
  name: string;
  performedBy: string[];  // Who calls this section
  performs: string[];     // What this section calls
}

interface ParagraphNode extends ASTNode {
  type: 'PARAGRAPH';
  name: string;
  performedBy: string[];
  performs: string[];
}
```

### 3. Parser Architecture

**Phase 1: Lexical Analysis**
- Tokenize COBOL source into meaningful tokens
- Handle COBOL's column-sensitive format (columns 7-72)
- Identify keywords, identifiers, literals, and operators

**Phase 2: Syntactic Analysis**
- Parse tokens into AST structure
- Focus on PROCEDURE DIVISION structure
- Identify sections (ending with SECTION keyword)
- Identify paragraphs (standalone labels)

**Phase 3: Semantic Analysis**
- Analyze PERFORM statements to build call relationships
- Handle different PERFORM variants:
  - `PERFORM paragraph-name`
  - `PERFORM section-name`
  - `PERFORM paragraph-name THROUGH paragraph-name`

### 4. Implementation Strategy

**Step 1: Basic Infrastructure**
- Set up TypeScript project with proper tooling
- Create CLI interface for file input/output
- Implement basic file reading and JSON writing

**Step 2: Simple Parser**
- Start with a basic line-by-line parser
- Identify PROCEDURE DIVISION boundaries
- Extract section and paragraph names using regex patterns

**Step 3: PERFORM Analysis**
- Parse PERFORM statements within each section/paragraph
- Build call relationship mappings
- Handle edge cases (THROUGH, nested performs)

**Step 4: AST Construction**
- Build complete AST structure
- Associate source line numbers with each node
- Generate final JSON output

### 5. Key Technical Considerations

**COBOL Specifics:**
- Handle continuation lines (- in column 7)
- Respect COBOL's case-insensitive nature
- Deal with fixed-format vs free-format source
- Handle comments (* in column 7)

**Error Handling:**
- Graceful handling of malformed COBOL
- Meaningful error messages with line numbers
- Partial parsing when possible

**Performance:**
- Efficient line-by-line processing for large files
- Minimal memory footprint for AST construction

### 6. Testing Strategy
- Unit tests for each parser component
- Integration tests with sample COBOL programs
- Edge case testing (nested performs, complex control flow)
- Validation against known COBOL program structures

### 7. Example Usage
```bash
npm run build
node dist/index.js path/to/program.cbl
# Generates: path/to/program.ast.json
```

This design provides a solid foundation that can be incrementally developed and easily extended to handle additional COBOL constructs beyond the initial PROCEDURE DIVISION focus.

Would you like me to help you start implementing this solution by setting up the project structure and initial files?

# PROMPT 2
Change the design to include the following requirements:
- the property sourceLines of ASTNode is an array of strings not an array of numbers and should contain the source code lines that belong to the section or paragraph.
- consider all variants of PERFORM statements, including those that represent loop constructs.
- limit unit tests to methods that are APIs of a class or module, not to internal methods.
- avoid using mocking libraries, use real data and real files instead.

# Response 2
Excellent feedback! Let me revise the design to incorporate these important requirements.

## Revised Design Recommendations

### 1. Updated AST Node Structure
```typescript
interface ASTNode {
  type: string;
  name?: string;
  sourceLines: string[];  // Changed: Array of actual source code lines
  children: ASTNode[];
}

interface ProgramNode extends ASTNode {
  type: 'PROGRAM';
  divisions: DivisionNode[];
}

interface ProcedureDivisionNode extends ASTNode {
  type: 'PROCEDURE_DIVISION';
  sections: SectionNode[];
  paragraphs: ParagraphNode[];
}

interface SectionNode extends ASTNode {
  type: 'SECTION';
  name: string;
  performedBy: string[];  // Who calls this section
  performs: PerformCall[]; // What this section calls (with context)
}

interface ParagraphNode extends ASTNode {
  type: 'PARAGRAPH';
  name: string;
  performedBy: string[];
  performs: PerformCall[];
}

// Enhanced PERFORM call representation
interface PerformCall {
  type: 'SIMPLE' | 'THROUGH' | 'TIMES' | 'UNTIL' | 'VARYING';
  target: string;
  throughTarget?: string;  // For PERFORM...THROUGH
  condition?: string;      // For UNTIL/WHILE conditions
  timesExpression?: string; // For PERFORM...TIMES
  varyingClause?: VaryingClause; // For PERFORM...VARYING
  sourceLineNumber: number; // Where this PERFORM appears
}

interface VaryingClause {
  variable: string;
  from: string;
  by: string;
  until: string;
}
```

### 2. Comprehensive PERFORM Statement Handling

**PERFORM Statement Variants to Support:**

1. **Simple PERFORM**
   ```cobol
   PERFORM PARAGRAPH-NAME
   PERFORM SECTION-NAME
   ```

2. **PERFORM THROUGH**
   ```cobol
   PERFORM PARA-1 THROUGH PARA-5
   PERFORM SECTION-1 THROUGH SECTION-3
   ```

3. **PERFORM TIMES (Loop)**
   ```cobol
   PERFORM PARAGRAPH-NAME 5 TIMES
   PERFORM PARAGRAPH-NAME COUNTER TIMES
   ```

4. **PERFORM UNTIL (Loop)**
   ```cobol
   PERFORM PARAGRAPH-NAME UNTIL WS-FLAG = 'Y'
   PERFORM PARAGRAPH-NAME WITH TEST BEFORE UNTIL END-OF-FILE
   PERFORM PARAGRAPH-NAME WITH TEST AFTER UNTIL WS-COUNT > 100
   ```

5. **PERFORM VARYING (Loop)**
   ```cobol
   PERFORM PARAGRAPH-NAME VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
   PERFORM PARAGRAPH-NAME VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MAX
                          AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 5
   ```

6. **Inline PERFORM**
   ```cobol
   PERFORM UNTIL WS-FLAG = 'Y'
       MOVE 'X' TO WS-TEMP
       ADD 1 TO WS-COUNTER
   END-PERFORM
   ```

### 3. Enhanced Parser Architecture

**Updated Phase 2: Syntactic Analysis**
```typescript
class CobolParser {
  // Public API methods (these will be unit tested)
  public parseProgram(sourceLines: string[]): ProgramNode
  public parseProcedureDivision(lines: string[], startIndex: number): ProcedureDivisionNode
  public extractPerformCalls(sourceLines: string[]): PerformCall[]
  
  // Private implementation methods (not directly unit tested)
  private identifyDivisions(lines: string[]): DivisionBoundary[]
  private parseSections(lines: string[], startIndex: number, endIndex: number): SectionNode[]
  private parseParagraphs(lines: string[], startIndex: number, endIndex: number): ParagraphNode[]
  private parsePerformStatement(line: string, lineNumber: number): PerformCall | null
}
```

**PERFORM Statement Parser**
```typescript
class PerformStatementParser {
  // Public API (unit tested)
  public parsePerformStatement(statement: string, lineNumber: number): PerformCall | null
  public isPerformStatement(line: string): boolean
  
  // Private methods handle specific PERFORM types
  private parseSimplePerform(statement: string): PerformCall
  private parsePerformThrough(statement: string): PerformCall
  private parsePerformTimes(statement: string): PerformCall
  private parsePerformUntil(statement: string): PerformCall
  private parsePerformVarying(statement: string): PerformCall
  private parseInlinePerform(statement: string, sourceLines: string[], startIndex: number): PerformCall
}
```

### 4. Revised Testing Strategy

**Unit Testing Approach (No Mocking, Real Data):**

```typescript
// Test real COBOL files in tests/fixtures/
tests/
├── fixtures/
│   ├── simple-program.cbl        # Basic program with simple PERFORMs
│   ├── perform-through.cbl       # PERFORM...THROUGH examples
│   ├── perform-loops.cbl         # TIMES, UNTIL, VARYING loops
│   ├── complex-program.cbl       # Real-world complexity
│   └── malformed-program.cbl     # Error handling test cases
├── parser.test.ts               # Test CobolParser public APIs
├── perform-parser.test.ts       # Test PerformStatementParser APIs
└── integration.test.ts          # End-to-end testing

// Example unit test structure
describe('CobolParser', () => {
  test('parseProgram should extract procedure division correctly', () => {
    const sourceLines = fs.readFileSync('tests/fixtures/simple-program.cbl', 'utf-8').split('\n');
    const parser = new CobolParser();
    const ast = parser.parseProgram(sourceLines);
    
    expect(ast.type).toBe('PROGRAM');
    expect(ast.divisions).toHaveLength(1);
    expect(ast.divisions[0].type).toBe('PROCEDURE_DIVISION');
  });

  test('extractPerformCalls should identify all PERFORM variants', () => {
    const sourceLines = fs.readFileSync('tests/fixtures/perform-loops.cbl', 'utf-8').split('\n');
    const parser = new CobolParser();
    const performCalls = parser.extractPerformCalls(sourceLines);
    
    // Verify specific PERFORM types are detected
    expect(performCalls.some(call => call.type === 'TIMES')).toBe(true);
    expect(performCalls.some(call => call.type === 'UNTIL')).toBe(true);
    expect(performCalls.some(call => call.type === 'VARYING')).toBe(true);
  });
});
```

**Test Fixtures Examples:**

```cobol
// tests/fixtures/perform-loops.cbl
IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-EXAMPLES.

PROCEDURE DIVISION.
MAIN-PROCESSING SECTION.
    PERFORM INIT-ROUTINE
    PERFORM PROCESS-RECORDS UNTIL END-OF-FILE = 'Y'
    PERFORM CLEANUP-ROUTINE 5 TIMES
    PERFORM CALCULATE-TOTALS VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
    PERFORM DISPLAY-HEADER THROUGH DISPLAY-FOOTER
    STOP RUN.

INIT-ROUTINE.
    MOVE 'N' TO END-OF-FILE.

PROCESS-RECORDS.
    READ INPUT-FILE
    AT END MOVE 'Y' TO END-OF-FILE.

CALCULATE-TOTALS.
    ADD WS-AMOUNT(WS-I) TO WS-TOTAL.

CLEANUP-ROUTINE.
    CLOSE ALL-FILES.

DISPLAY-HEADER.
    DISPLAY 'REPORT HEADER'.

DISPLAY-DETAIL.
    DISPLAY WS-DETAIL-LINE.

DISPLAY-FOOTER.
    DISPLAY 'REPORT FOOTER'.
```

### 5. Enhanced Source Line Management

**Source Line Collection Strategy:**
```typescript
interface SourceLineManager {
  collectSectionLines(sectionName: string, allLines: string[], sectionStart: number, sectionEnd: number): string[]
  collectParagraphLines(paragraphName: string, allLines: string[], paragraphStart: number, paragraphEnd: number): string[]
  cleanSourceLine(line: string): string  // Remove sequence numbers, handle continuations
}
```

**Benefits of String Array for sourceLines:**
- Preserves original formatting and comments
- Enables better debugging and traceability
- Allows for syntax highlighting in output tools
- Maintains COBOL column structure information
- Facilitates code generation and refactoring tools

### 6. Integration Testing with Real Files

```typescript
describe('End-to-End Integration', () => {
  test('complete parsing of real COBOL program', () => {
    const inputFile = 'tests/fixtures/complex-program.cbl';
    const expectedOutputFile = 'tests/fixtures/complex-program.expected.json';
    
    // Run the actual parser
    execSync(`node dist/index.js ${inputFile}`);
    
    // Compare with expected output
    const actualOutput = JSON.parse(fs.readFileSync(`${inputFile}.ast.json`, 'utf-8'));
    const expectedOutput = JSON.parse(fs.readFileSync(expectedOutputFile, 'utf-8'));
    
    expect(actualOutput).toEqual(expectedOutput);
  });
});
```

This revised design provides more comprehensive PERFORM statement handling, uses real source code lines for better traceability, focuses testing on public APIs with real data, and eliminates the complexity of mocking libraries while maintaining thorough test coverage.


# QUESTION 3
Do you suggest to use a parser generator like ANTLR or PEG.js for this project, or is it better to implement the parser manually in TypeScript?

# Response 3
GitHub Copilot: Great question! Let me analyze the pros and cons for this specific COBOL AST parser project.

## Analysis: Parser Generator vs Manual Implementation

### For This Specific Project: **Manual TypeScript Implementation** is Recommended

Here's why:

### 1. **Limited Scope Favors Manual Parsing**
- You're only parsing **PROCEDURE DIVISION** initially
- Focus is on **sections, paragraphs, and PERFORM statements**
- This is a subset of COBOL, not a full language parser
- Manual parsing will be simpler and more maintainable for this scope

### 2. **COBOL's Unique Challenges**
```cobol
000100 IDENTIFICATION DIVISION.                    
000200 PROGRAM-ID. SAMPLE.                        
000300                                           
000400 PROCEDURE DIVISION.                        
000500 MAIN-SECTION SECTION.                     
000600     PERFORM INIT-ROUTINE                  
000700-        UNTIL END-OF-FILE = 'Y'          
000800     STOP RUN.                            
```

**COBOL-specific issues that favor manual parsing:**
- **Column-sensitive format** (sequence area, indicator area, A/B margins)
- **Continuation lines** (- in column 7)
- **Case insensitivity** with mixed case in real code
- **Flexible spacing** and **optional periods**
- **Context-dependent keywords** (SECTION can be a keyword or part of a name)

### 3. **Parser Generator Overhead**
With ANTLR or PEG.js you'd need:
```grammar
// Complex grammar for just PROCEDURE DIVISION subset
procedure_division: 'PROCEDURE' 'DIVISION' '.'? section_or_paragraph*
section: IDENTIFIER 'SECTION' '.'? paragraph*
paragraph: IDENTIFIER '.'? statement*
perform_stmt: 'PERFORM' (simple_perform | through_perform | times_perform | until_perform | varying_perform)
// ... hundreds of lines for complete PERFORM syntax
```

**Problems:**
- Grammar complexity for marginal benefit
- Additional build step and tooling
- Harder to debug parsing issues
- More dependencies and learning curve

### 4. **Recommended Manual Implementation Strategy**

```typescript
// Clean, focused manual parser
export class CobolParser {
  private lines: string[];
  private currentIndex: number = 0;

  public parseProgram(sourceCode: string): ProgramNode {
    this.lines = this.preprocessLines(sourceCode.split('\n'));
    return this.buildAST();
  }

  private preprocessLines(lines: string[]): string[] {
    return lines
      .map(line => this.cleanLine(line))
      .filter(line => !this.isComment(line) && line.trim().length > 0)
      .map(line => this.handleContinuation(line));
  }

  private cleanLine(line: string): string {
    // Handle COBOL column structure
    if (line.length < 7) return line;
    return line.substring(6).trimEnd(); // Skip sequence area
  }

  private findProcedureDivision(): number {
    for (let i = 0; i < this.lines.length; i++) {
      if (this.lines[i].toUpperCase().includes('PROCEDURE DIVISION')) {
        return i;
      }
    }
    throw new Error('PROCEDURE DIVISION not found');
  }

  private parseProcedureDivision(startIndex: number): ProcedureDivisionNode {
    const elements = this.extractSectionsAndParagraphs(startIndex);
    return {
      type: 'PROCEDURE_DIVISION',
      sourceLines: this.getSourceLines(startIndex, this.lines.length),
      sections: elements.sections,
      paragraphs: elements.paragraphs,
      children: [...elements.sections, ...elements.paragraphs]
    };
  }

  private parsePerformStatement(line: string, lineNumber: number): PerformCall | null {
    const trimmed = line.trim().toUpperCase();
    if (!trimmed.startsWith('PERFORM ')) return null;

    // Simple regex-based parsing for different PERFORM types
    if (this.isPerformThrough(trimmed)) {
      return this.parsePerformThrough(trimmed, lineNumber);
    } else if (this.isPerformTimes(trimmed)) {
      return this.parsePerformTimes(trimmed, lineNumber);
    } else if (this.isPerformUntil(trimmed)) {
      return this.parsePerformUntil(trimmed, lineNumber);
    } else if (this.isPerformVarying(trimmed)) {
      return this.parsePerformVarying(trimmed, lineNumber);
    } else {
      return this.parseSimplePerform(trimmed, lineNumber);
    }
  }
}
```

### 5. **When You WOULD Use a Parser Generator**

Parser generators make sense for:
- **Full COBOL compiler** (all divisions, all statements)
- **Complex transformation tools** requiring deep syntax analysis
- **Projects with dedicated parser expertise** and time
- **When you need error recovery** and detailed syntax error reporting

### 6. **Advantages of Manual Approach for Your Project**

**Simplicity:**
```typescript
// Easy to understand and modify
if (line.toUpperCase().includes('PERFORM') && line.toUpperCase().includes('UNTIL')) {
  return this.parsePerformUntil(line, lineNumber);
}
```

**Flexibility:**
```typescript
// Easy to handle COBOL quirks
private handleCobolQuirks(line: string): string {
  // Remove trailing periods that are optional
  line = line.replace(/\.$/, '');
  // Handle embedded comments
  if (line.includes('*>')) {
    line = line.substring(0, line.indexOf('*>'));
  }
  return line.trim();
}
```

**Debugging:**
- Step through code with regular debugger
- Add logging at any point
- Easy to test individual parsing functions

**Incremental Development:**
- Start with basic section/paragraph detection
- Add PERFORM parsing incrementally
- Easy to extend for new requirements

### 7. **Recommended Architecture**

```typescript
src/
├── parser/
│   ├── cobol-parser.ts           # Main parser class
│   ├── line-processor.ts         # Handle COBOL line format
│   ├── perform-parser.ts         # Specialized PERFORM parsing
│   ├── section-parser.ts         # Section detection
│   └── paragraph-parser.ts       # Paragraph detection
├── types/
│   └── ast-types.ts             # All AST node definitions
└── utils/
    ├── cobol-utils.ts           # COBOL-specific utilities
    └── string-utils.ts          # General string processing
```

### **Conclusion**

For your specific requirements (PROCEDURE DIVISION focus, PERFORM statement analysis, limited scope), a **manual TypeScript implementation** will be:
- **Faster to develop** and iterate
- **Easier to debug** and maintain
- **More flexible** for COBOL's unique characteristics
- **Sufficient** for the parsing complexity you need

Save parser generators for when you need to parse the entire COBOL language or require sophisticated error recovery.


# QUESTION 4
In the next future I want to extend the parse to support the DATA DIVISION and the FILE SECTION, so I will need to parse the COBOL data definitions. What I want to parse are the definitions of the data items, their types, and their relationships. In this case do you still suggest to implement a custom parser in TypeScript or to use a parser generator like ANTLR or PEG.js?

# Response 4
Excellent question! With the addition of DATA DIVISION and FILE SECTION parsing, this significantly changes the complexity analysis. Let me reassess the recommendation.

## Revised Analysis: With DATA DIVISION Support

### **New Recommendation: Consider PEG.js (with Manual as Viable Alternative)**

The addition of DATA DIVISION parsing introduces substantially more complexity:

### 1. **DATA DIVISION Complexity Examples**

```cobol
DATA DIVISION.
FILE SECTION.
FD  CUSTOMER-FILE
    RECORDING MODE IS F
    LABEL RECORDS ARE STANDARD
    RECORD CONTAINS 100 CHARACTERS.

01  CUSTOMER-RECORD.
    05  CUST-ID                PIC 9(6).
    05  CUST-NAME              PIC X(30).
    05  CUST-ADDRESS.
        10  STREET             PIC X(25).
        10  CITY               PIC X(20).
        10  STATE              PIC XX.
        10  ZIP-CODE           PIC 9(5).
    05  CUST-BALANCE           PIC S9(7)V99 COMP-3.
    05  CUST-STATUS            PIC X.
        88  ACTIVE             VALUE 'A'.
        88  INACTIVE           VALUE 'I'.
        88  SUSPENDED          VALUE 'S'.
    05  TRANSACTION-TABLE.
        10  TRANSACTION-COUNT  PIC 9(3).
        10  TRANSACTIONS       OCCURS 1 TO 50 TIMES
                              DEPENDING ON TRANSACTION-COUNT.
            15  TRANS-DATE     PIC 9(8).
            15  TRANS-TYPE     PIC X.
            15  TRANS-AMOUNT   PIC S9(7)V99.

WORKING-STORAGE SECTION.
01  WS-COUNTERS.
    05  WS-RECORD-COUNT        PIC 9(6) VALUE ZERO.
    05  WS-ERROR-COUNT         PIC 9(4) VALUE ZERO.

01  WS-FLAGS.
    05  WS-EOF-FLAG            PIC X VALUE 'N'.
        88  END-OF-FILE        VALUE 'Y'.
    05  WS-ERROR-FLAG          PIC X VALUE 'N'.

01  WS-WORK-AREAS.
    05  WS-CURRENT-DATE        PIC 9(8).
    05  WS-FORMATTED-DATE      PIC X(10).
    05  WS-TEMP-AMOUNT         PIC S9(7)V99 COMP.
```

### 2. **Complex Parsing Challenges in DATA DIVISION**

**Level Number Hierarchy:**
- Nested structures with levels 01-49, 66, 77, 88
- Complex parent-child relationships
- Variable indentation and spacing

**PIC Clauses:**
```cobol
PIC 9(6)           # Numeric
PIC X(30)          # Alphanumeric  
PIC S9(7)V99       # Signed with decimal
PIC 9(3) COMP-3    # Packed decimal
PIC S9(4) COMP     # Binary
PIC A(20)          # Alphabetic
PIC 999.99         # Edited numeric
PIC $ZZ,ZZ9.99     # Currency formatted
```

**Complex Clauses:**
```cobol
OCCURS 1 TO 50 TIMES DEPENDING ON TRANSACTION-COUNT
INDEXED BY WS-INDEX
ASCENDING KEY IS SORT-KEY
REDEFINES OTHER-FIELD
SYNCHRONIZED LEFT
JUSTIFIED RIGHT
BLANK WHEN ZERO
```

**88-Level Conditions:**
```cobol
88  VALID-STATUS-VALUES  VALUE 'A', 'I', 'S'.
88  HIGH-BALANCE         VALUE 10000.00 THRU 99999.99.
```

### 3. **Complexity Comparison**

| Aspect | PROCEDURE DIVISION Only | + DATA DIVISION |
|--------|------------------------|-----------------|
| Grammar Rules | ~20 rules | ~100+ rules |
| Parsing Logic | Simple patterns | Complex hierarchical |
| Error Handling | Basic | Sophisticated needed |
| Context Sensitivity | Minimal | High (level numbers, scoping) |
| Maintenance | Easy | Complex |

### 4. **PEG.js Recommendation for Extended Scope**

```javascript
// PEG.js grammar becomes manageable for complex data structures
data_division = "DATA" "DIVISION" "."? section*

file_section = "FILE" "SECTION" "."? fd_entry*

fd_entry = "FD" file_name clause* "." record_description+

record_description = level_number data_name? pic_clause? occurs_clause? 
                    value_clause? redefines_clause? other_clauses*

level_number = ("01" / "02" / "03" / ... / "49" / "66" / "77" / "88")

pic_clause = "PIC" "IS"? picture_string

picture_string = [X9AS$Z.,+-]+ ("(" digits ")")?

occurs_clause = "OCCURS" (fixed_occurs / variable_occurs)

variable_occurs = digits "TO" digits "TIMES" 
                 ("DEPENDING" "ON" data_name)?
                 ("INDEXED" "BY" index_name+)?
                 ("ASCENDING" / "DESCENDING" "KEY" "IS" data_name+)?

value_clause = "VALUE" "IS"? (literal / "ZERO" / "SPACE" / "HIGH-VALUE" / etc)

condition_name = "88" condition_name_entry value_clause
```

### 5. **Hybrid Approach Recommendation**

**Option A: PEG.js for DATA DIVISION, Manual for PROCEDURE**
```typescript
// Use PEG.js generated parser for complex data structures
import { parse as parseDataDivision } from './grammar/data-division-parser';

class CobolParser {
  public parseProgram(sourceCode: string): ProgramNode {
    const divisions = this.identifyDivisions(sourceCode);
    
    return {
      type: 'PROGRAM',
      dataDivision: parseDataDivision(divisions.dataDivision), // PEG.js
      procedureDivision: this.parseProcedureDivision(divisions.procedureDivision), // Manual
      sourceLines: divisions.all,
      children: []
    };
  }
  
  // Keep manual parsing for PROCEDURE DIVISION (it's working well)
  private parseProcedureDivision(lines: string[]): ProcedureDivisionNode {
    // ...existing manual logic...
  }
}
```

**Option B: Full PEG.js Implementation**
```typescript
// Complete grammar-based approach
import { parse as parseCobol } from './grammar/cobol-parser';

class CobolParser {
  public parseProgram(sourceCode: string): ProgramNode {
    try {
      return parseCobol(sourceCode);
    } catch (error) {
      return this.handleParsingError(error, sourceCode);
    }
  }
}
```

### 6. **Updated Project Structure**

```
src/
├── grammar/                    # PEG.js grammars
│   ├── cobol.pegjs            # Main COBOL grammar
│   ├── data-division.pegjs    # DATA DIVISION specific
│   └── generated/             # Generated parsers
├── parser/
│   ├── cobol-parser.ts        # Main orchestrator
│   ├── manual-parsers/        # Manual parsers for simple parts
│   │   └── procedure-parser.ts
│   └── ast-builder.ts         # Convert parse tree to AST
├── types/
│   ├── ast-types.ts           # AST node definitions
│   └── data-types.ts          # DATA DIVISION specific types
└── utils/
    ├── cobol-preprocessor.ts  # Line cleaning, continuations
    └── error-handler.ts       # Parsing error management
```

### 7. **Data Division AST Structure**

```typescript
interface DataDivisionNode extends ASTNode {
  type: 'DATA_DIVISION';
  fileSections: FileSectionNode[];
  workingStorageSection?: WorkingStorageSectionNode;
  linkageSection?: LinkageSectionNode;
}

interface DataItemNode extends ASTNode {
  type: 'DATA_ITEM';
  level: number;
  name: string;
  picture?: string;
  usage?: string;
  value?: string;
  occurs?: OccursClause;
  redefines?: string;
  conditions?: ConditionNode[]; // 88-level items
  parent?: DataItemNode;
  children: DataItemNode[];
}

interface OccursClause {
  minOccurs?: number;
  maxOccurs: number;
  dependingOn?: string;
  indexedBy?: string[];
  keys?: KeyClause[];
}
```

### 8. **Final Recommendation**

Given the extended requirements for DATA DIVISION:

**Go with PEG.js** because:
- DATA DIVISION parsing is genuinely complex
- Hierarchical data structures need proper grammar support
- PIC clause parsing has many edge cases
- OCCURS, REDEFINES, and condition names create complex relationships
- You'll need proper error reporting for malformed data definitions
- Grammar-based approach will be more maintainable long-term

**Implementation Strategy:**
1. Start with PEG.js for DATA DIVISION only
2. Keep existing manual PROCEDURE DIVISION parser
3. Gradually migrate PROCEDURE DIVISION to PEG.js if beneficial
4. Use robust preprocessing for COBOL format quirks

The complexity jump from PROCEDURE DIVISION only to including DATA DIVISION crosses the threshold where a parser generator becomes clearly beneficial.


# QUESTION 5
Let's go back to the original assumption of only parsing the PROCEDURE DIVISION. Show me how you would implement it with an ANTLR parser assuming to use antlr4ng library. Just describe the design. Do not implement anything.

# Response 5

## ANTLR4 Design for PROCEDURE DIVISION Parser

### 1. **Overall Architecture**

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   COBOL Source  │───►│   Preprocessor   │───►│ ANTLR4 Parser  │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                         │
                                                         ▼
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   JSON Output   │◄───│   AST Builder    │◄───│   Parse Tree    │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

### 2. **ANTLR Grammar Strategy**

**Grammar File: `CobolProcedure.g4`**
- **Focus**: Only PROCEDURE DIVISION constructs
- **Scope**: Sections, paragraphs, and all PERFORM variants
- **Approach**: Hierarchical grammar with separate rules for each PERFORM type

**Key Grammar Rules:**
```antlr
procedureDivision   : PROCEDURE DIVISION DOT? procedureElement*
procedureElement    : section | paragraph | statement
section            : sectionName SECTION DOT? paragraph*
paragraph          : paragraphName DOT statement*
performStatement   : simplePerform | performThrough | performTimes 
                   | performUntil | performVarying | inlinePerform
```

**Lexer Strategy:**
- Case-insensitive keywords using character fragments
- Handle COBOL-specific punctuation (optional periods)
- Support for hyphenated identifiers
- String and numeric literals
- Flexible whitespace handling

### 3. **Preprocessing Layer**

**Purpose**: Handle COBOL format quirks before ANTLR parsing

**Responsibilities:**
- **Column Management**: Extract content from columns 8-72
- **Comment Removal**: Skip `*` and `/` comment lines
- **Continuation Lines**: Join lines with `-` in column 7
- **Division Extraction**: Isolate PROCEDURE DIVISION content
- **Whitespace Normalization**: Clean up spacing

**Input/Output:**
- Input: Raw COBOL source with all format complexities
- Output: Clean text suitable for ANTLR parsing

### 4. **ANTLR Integration Pattern**

**Library**: `antlr4ng` for TypeScript
**Generation**: Use ANTLR CLI to generate TypeScript parser files
**Visitor Pattern**: Implement custom visitor to traverse parse tree

**Generated Files (auto-generated):**
```
generated/
├── CobolProcedureLexer.ts      # Token recognition
├── CobolProcedureParser.ts     # Grammar rules
├── CobolProcedureVisitor.ts    # Base visitor interface
└── CobolProcedureListener.ts   # Base listener interface
```

### 5. **AST Builder Design**

**Pattern**: Visitor pattern extending generated base visitor
**Purpose**: Convert ANTLR parse tree to custom AST structure

**Key Methods:**
```typescript
class ASTBuilderVisitor extends CobolProcedureBaseVisitor<ASTNode> {
  visitProcedureDivision(ctx: ProcedureDivisionContext): ProcedureDivisionNode
  visitSection(ctx: SectionContext): SectionNode
  visitParagraph(ctx: ParagraphContext): ParagraphNode
  visitPerformStatement(ctx: PerformStatementContext): PerformStatementNode
  visitSimplePerform(ctx: SimplePerformContext): PerformCall
  visitPerformThrough(ctx: PerformThroughContext): PerformCall
  // ... methods for each PERFORM variant
}
```

**Responsibilities:**
- Extract source line ranges for each AST node
- Build parent-child relationships
- Collect PERFORM call information
- Handle error recovery and partial parsing

### 6. **PERFORM Statement Handling**

**Grammar Coverage:**
- **Simple**: `PERFORM PARAGRAPH-NAME`
- **Through**: `PERFORM PARA-1 THROUGH PARA-5`
- **Times**: `PERFORM PARAGRAPH-NAME 5 TIMES`
- **Until**: `PERFORM PARAGRAPH-NAME UNTIL condition`
- **Varying**: `PERFORM PARAGRAPH-NAME VARYING...`
- **Inline**: `PERFORM UNTIL condition ... END-PERFORM`

**Complex Parsing Examples:**
```antlr
performUntil : PERFORM procedureName 
               (WITH TEST (BEFORE | AFTER))? 
               UNTIL condition ;

performVarying : PERFORM procedureName 
                 VARYING varyingClause 
                 (AFTER varyingClause)* ;

condition : identifier comparisonOperator literal
          | identifier  
          | NOT condition
          | condition (AND | OR) condition ;
```

### 7. **Error Handling Strategy**

**ANTLR Error Recovery:**
- Custom error listener for meaningful error messages
- Error recovery rules for partial parsing
- Line number and column tracking

**Graceful Degradation:**
- Continue parsing after errors when possible
- Mark problematic sections in AST
- Provide diagnostic information

### 8. **Main Parser Orchestrator**

**Class: `CobolParser`**
```typescript
class CobolParser {
  private preprocessor: CobolPreprocessor;
  private astBuilder: ASTBuilderVisitor;
  
  public parseProgram(sourceCode: string): ProgramNode {
    // 1. Preprocess COBOL source
    // 2. Create ANTLR input stream
    // 3. Generate tokens with lexer
    // 4. Parse with generated parser
    // 5. Visit parse tree to build AST
    // 6. Analyze PERFORM relationships
    // 7. Return complete AST
  }
}
```

### 9. **Build Process Integration**

**NPM Scripts:**
```json
{
  "scripts": {
    "generate-parser": "antlr4ng -Dlanguage=TypeScript -visitor src/grammar/CobolProcedure.g4 -o src/generated",
    "build": "npm run generate-parser && tsc",
    "dev": "npm run generate-parser && tsc --watch"
  }
}
```

**Development Workflow:**
1. Modify grammar file
2. Regenerate parser files
3. Update AST builder if needed
4. Test with sample COBOL files

### 10. **Testing Strategy**

**Grammar Testing:**
- Test individual grammar rules with ANTLR TestRig
- Validate parse trees for various PERFORM constructs
- Test error recovery with malformed input

**Integration Testing:**
- Real COBOL files as test fixtures
- Compare generated AST with expected output
- Performance testing with large procedures

### 11. **Advantages of ANTLR Approach**

**Grammar Benefits:**
- Declarative grammar is self-documenting
- Easy to extend for additional COBOL constructs
- Built-in error recovery and reporting
- Automatic parse tree generation

**Maintenance Benefits:**
- Grammar changes don't require manual parser updates
- ANTLR handles complex precedence and associativity
- Rich tooling ecosystem (grammar visualization, debugging)

**Robustness:**
- Well-tested parsing algorithms
- Handles left-recursion and ambiguity
- Professional-grade error handling

### 12. **Potential Challenges**

**COBOL-Specific Issues:**
- Optional periods require careful grammar design
- Case-insensitive keywords need fragment definitions
- Context-sensitive parsing (SECTION vs section-name)

**Performance Considerations:**
- ANTLR overhead for simple parsing tasks
- Memory usage for large parse trees
- Cold start time for parser initialization

**Complexity:**
- Additional build step and toolchain
- Learning curve for ANTLR grammar syntax
- Debugging parse issues requires ANTLR knowledge

This design provides a robust, maintainable foundation for parsing PROCEDURE DIVISION while leveraging ANTLR's powerful parsing capabilities and error handling.

Similar code found with 2 license types


# QUESTION 6
Ok. I have decided to implement a custom typescript parser without using any AST parsing library. Go back to the requirements described in the README.md file in the context and generate an implementation plan in the /plans folder. Do not implement any code yet. Just write a detailed implementation plan in markdown format.

# Response 6
I'll create a detailed implementation plan for building a custom TypeScript parser for COBOL PROCEDURE DIVISION without using any AST parsing libraries.

Created implementation-plan.md

I've created a comprehensive implementation plan for building a custom TypeScript COBOL parser. The plan is structured in 7 phases, covering everything from basic project setup to final testing and validation.

Key highlights of the plan:

Architecture: Modular design with separate components for line processing, parsing, AST building, and output formatting.

Phased Approach:

Project setup and infrastructure
COBOL line format handling
Basic structure parsing (sections/paragraphs)
PERFORM statement analysis
AST construction
JSON output and CLI integration
Comprehensive testing
Technical Focus: The plan addresses COBOL-specific challenges like:

Fixed-format columns (7-72)
Continuation lines and comments
Case-insensitive parsing
Optional punctuation
Various PERFORM statement types
Deliverables: Each phase has clear deliverables and estimated duration, totaling 17-24 days of development time.

The plan emphasizes test-driven development with real COBOL code samples and includes comprehensive error handling strategies. It's designed to be maintainable and extensible for future enhancements like DATA DIVISION support.

Would you like me to elaborate on any specific phase or aspect of the implementation plan?


# QUESTION 7
I have updated the requirements in the README.md file, adding a section about how to deal with the Cobol COPY statements. 

# Response 7
I can see the updated requirements in the README.md file. The addition of COPY statement handling significantly changes the complexity and design of the parser. Let me update the implementation plan to incorporate these new requirements.


I've successfully updated the implementation plan to incorporate the new COPY statement requirements. Here are the key changes made:

Major Additions to the Plan:
1. New Core Component: COPY Resolution System
Added CopyResolver class as a major component
Dedicated phase (Phase 2) for COPY statement handling
Support for recursive file inclusion with circular dependency detection
2. Enhanced AST Structure with Source Line Tracking
Implemented the suggested approach of modeling each line as an AST node
Added SourceLineNode interface with file attribution
Enhanced all AST nodes to track source file origins
Added PerformReference with cross-file context
3. Cross-File Functionality
Enhanced all parsing phases to handle multi-file scenarios
PERFORM analysis across file boundaries
Symbol resolution across included files
Source line attribution throughout the process
4. Expanded File Structure
New /copy directory for COPY-related functionality
Enhanced test structure with complex multi-file scenarios
Additional documentation for COPY handling
5. Comprehensive Testing Strategy
Deep nesting scenarios (3-4 levels of COPY inclusion)
Circular dependency detection testing
Cross-file PERFORM reference validation
Performance testing with large COPYBOOK libraries
6. Updated Duration and Complexity
Increased from 17-24 days to 26-32 days due to COPY complexity
More detailed phases with COPY-specific considerations
Enhanced error handling and validation requirements
Key Technical Features Added:
Recursive COPY Resolution: Handle nested COPY statements
Source File Tracking: Every line knows its origin file
Circular Dependency Detection: Prevent infinite inclusion loops
Cross-File Name Resolution: PERFORM targets in any included file
Path Resolution: Standard COPYBOOK library support
Enhanced Error Context: File inclusion chain in error messages
The plan now comprehensively addresses the COPY requirements while maintaining the original PROCEDURE DIVISION parsing goals. The source line tracking using AST nodes provides the flexibility you requested for maintaining file origin information throughout the parsing process.


