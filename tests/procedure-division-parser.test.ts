import { Logger } from '../src/utils/logger';
import { ProcedureDivisionParser } from '../src/parser/procedure-division-parser';
import { ProcessedLine } from '../src/types';

describe('ProcedureDivisionParser', () => {
  let parser: ProcedureDivisionParser;
  let mockLogger: jest.Mocked<Logger>;

  beforeEach(() => {
    mockLogger = new Logger() as jest.Mocked<Logger>;
    jest.spyOn(mockLogger, 'info');
    jest.spyOn(mockLogger, 'warn');
    jest.spyOn(mockLogger, 'error');
    jest.spyOn(mockLogger, 'verbose');
    jest.spyOn(mockLogger, 'success');
    jest.spyOn(mockLogger, 'setVerbose');
    
    parser = new ProcedureDivisionParser(mockLogger);
  });

  const createProcessedLine = (
    content: string,
    lineNumber: number,
    lineType: 'CODE' | 'COMMENT' | 'BLANK' | 'CONTINUATION' = 'CODE'
  ): ProcessedLine => ({
    content,
    originalLineNumber: lineNumber,
    sourceFile: 'test.cob',
    isFromCopy: false,
    lineType,
    rawContent: content,
    indicatorArea: ' ',
    areaA: content.substring(0, 4).trim(),
    areaB: content.substring(4).trim(),
  });

  describe('parseProcedureDivision', () => {
    it('should return null when no PROCEDURE DIVISION is found', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('IDENTIFICATION DIVISION.', 1),
        createProcessedLine('PROGRAM-ID. TEST.', 2),
        createProcessedLine('DATA DIVISION.', 3),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).toBeNull();
      expect(mockLogger.verbose).toHaveBeenCalledWith('No PROCEDURE DIVISION found');
    });

    it('should parse PROCEDURE DIVISION with simple paragraphs', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('DATA DIVISION.', 1),
        createProcessedLine('PROCEDURE DIVISION.', 2),
        createProcessedLine('MAIN-PARA.', 3),
        createProcessedLine('    DISPLAY "Hello".', 4),
        createProcessedLine('    STOP RUN.', 5),
        createProcessedLine('SUB-PARA.', 6),
        createProcessedLine('    DISPLAY "World".', 7),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.type).toBe('PROCEDURE_DIVISION');
      expect(result!.sections).toHaveLength(0);
      expect(result!.paragraphs).toHaveLength(2);
      
      expect(result!.paragraphs[0].name).toBe('MAIN-PARA');
      expect(result!.paragraphs[0].sourceLines).toHaveLength(3); // Declaration + 2 code lines
      
      expect(result!.paragraphs[1].name).toBe('SUB-PARA');
      expect(result!.paragraphs[1].sourceLines).toHaveLength(2); // Declaration + 1 code line
    });

    it('should parse PROCEDURE DIVISION with sections only', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('MAIN-SECTION SECTION.', 2),
        createProcessedLine('    DISPLAY "In main section".', 3),
        createProcessedLine('SUB-SECTION SECTION.', 4),
        createProcessedLine('    DISPLAY "In sub section".', 5),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(2);
      expect(result!.paragraphs).toHaveLength(0);
      
      expect(result!.sections[0].name).toBe('MAIN-SECTION');
      expect(result!.sections[0].paragraphs).toHaveLength(0);
      expect(result!.sections[0].sourceLines).toHaveLength(2);
      
      expect(result!.sections[1].name).toBe('SUB-SECTION');
      expect(result!.sections[1].sourceLines).toHaveLength(2);
    });

    it('should parse PROCEDURE DIVISION with sections containing paragraphs', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('MAIN-SECTION SECTION.', 2),
        createProcessedLine('FIRST-PARA.', 3),
        createProcessedLine('    DISPLAY "First".', 4),
        createProcessedLine('SECOND-PARA.', 5),
        createProcessedLine('    DISPLAY "Second".', 6),
        createProcessedLine('SUB-SECTION SECTION.', 7),
        createProcessedLine('THIRD-PARA.', 8),
        createProcessedLine('    DISPLAY "Third".', 9),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(2);
      expect(result!.paragraphs).toHaveLength(0); // All paragraphs are within sections
      
      expect(result!.sections[0].name).toBe('MAIN-SECTION');
      expect(result!.sections[0].paragraphs).toHaveLength(2);
      expect(result!.sections[0].paragraphs[0].name).toBe('FIRST-PARA');
      expect(result!.sections[0].paragraphs[1].name).toBe('SECOND-PARA');
      
      expect(result!.sections[1].name).toBe('SUB-SECTION');
      expect(result!.sections[1].paragraphs).toHaveLength(1);
      expect(result!.sections[1].paragraphs[0].name).toBe('THIRD-PARA');
    });

    it('should parse mixed sections and standalone paragraphs', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('STANDALONE-PARA.', 2),
        createProcessedLine('    DISPLAY "Standalone".', 3),
        createProcessedLine('MAIN-SECTION SECTION.', 4),
        createProcessedLine('SECTION-PARA.', 5),
        createProcessedLine('    DISPLAY "In section".', 6),
        createProcessedLine('ANOTHER-SECTION-PARA.', 7),
        createProcessedLine('    DISPLAY "Another in section".', 8),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(1);
      expect(result!.paragraphs).toHaveLength(1); // One standalone paragraph
      
      expect(result!.paragraphs[0].name).toBe('STANDALONE-PARA');
      expect(result!.sections[0].name).toBe('MAIN-SECTION');
      expect(result!.sections[0].paragraphs).toHaveLength(2);
      expect(result!.sections[0].paragraphs[0].name).toBe('SECTION-PARA');
      expect(result!.sections[0].paragraphs[1].name).toBe('ANOTHER-SECTION-PARA');
    });

    it('should handle PROCEDURE DIVISION with USING clause', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION USING PARAM1 PARAM2.', 1),
        createProcessedLine('MAIN-PARA.', 2),
        createProcessedLine('    DISPLAY "Hello".', 3),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.paragraphs).toHaveLength(1);
      expect(result!.paragraphs[0].name).toBe('MAIN-PARA');
    });

    it('should ignore comments and blank lines', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('*This is a comment', 2, 'COMMENT'),
        createProcessedLine('', 3, 'BLANK'),
        createProcessedLine('MAIN-PARA.', 4),
        createProcessedLine('*Another comment', 5, 'COMMENT'),
        createProcessedLine('    DISPLAY "Hello".', 6),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.paragraphs).toHaveLength(1);
      expect(result!.paragraphs[0].name).toBe('MAIN-PARA');
      expect(result!.paragraphs[0].sourceLines).toHaveLength(3); // Declaration + comment + code
    });

    it('should handle empty sections and paragraphs', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('EMPTY-SECTION SECTION.', 2),
        createProcessedLine('EMPTY-PARA.', 3),
        createProcessedLine('NON-EMPTY-PARA.', 4),
        createProcessedLine('    DISPLAY "Content".', 5),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(1);
      expect(result!.sections[0].name).toBe('EMPTY-SECTION');
      expect(result!.sections[0].paragraphs).toHaveLength(2);
      expect(result!.sections[0].paragraphs[0].name).toBe('EMPTY-PARA');
      expect(result!.sections[0].paragraphs[1].name).toBe('NON-EMPTY-PARA');
    });

    it('should handle complex paragraph names with hyphens and numbers', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('L000-MAIN-START.', 2),
        createProcessedLine('    DISPLAY "Start".', 3),
        createProcessedLine('L100-PROCESS-DATA.', 4),
        createProcessedLine('    DISPLAY "Processing".', 5),
        createProcessedLine('L999-END-PROGRAM.', 6),
        createProcessedLine('    STOP RUN.', 7),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.paragraphs).toHaveLength(3);
      expect(result!.paragraphs[0].name).toBe('L000-MAIN-START');
      expect(result!.paragraphs[1].name).toBe('L100-PROCESS-DATA');
      expect(result!.paragraphs[2].name).toBe('L999-END-PROGRAM');
    });
  });

  describe('validateStructure', () => {
    it('should validate correct structure without errors', () => {
      const procedureDivision = {
        type: 'PROCEDURE_DIVISION' as const,
        children: [],
        sections: [
          {
            type: 'SECTION' as const,
            name: 'MAIN-SECTION',
            children: [],
            paragraphs: [
              {
                type: 'PARAGRAPH' as const,
                name: 'MAIN-PARA',
                children: [],
                sourceLines: [createProcessedLine('MAIN-PARA.', 1)],
                performedBy: [],
                performs: [],
              }
            ],
            sourceLines: [createProcessedLine('MAIN-SECTION SECTION.', 1)],
            performedBy: [],
            performs: [],
          }
        ],
        paragraphs: [
          {
            type: 'PARAGRAPH' as const,
            name: 'STANDALONE-PARA',
            children: [],
            sourceLines: [createProcessedLine('STANDALONE-PARA.', 1)],
            performedBy: [],
            performs: [],
          }
        ],
        sourceLines: [createProcessedLine('PROCEDURE DIVISION.', 1)],
      };

      const errors = parser.validateStructure(procedureDivision);

      expect(errors).toHaveLength(0);
    });

    it('should report error for empty procedure division', () => {
      const procedureDivision = {
        type: 'PROCEDURE_DIVISION' as const,
        children: [],
        sections: [],
        paragraphs: [],
        sourceLines: [createProcessedLine('PROCEDURE DIVISION.', 1)],
      };

      const errors = parser.validateStructure(procedureDivision);

      expect(errors).toContain('PROCEDURE DIVISION contains no sections or paragraphs');
    });

    it('should report errors for invalid names', () => {
      const procedureDivision = {
        type: 'PROCEDURE_DIVISION' as const,
        children: [],
        sections: [
          {
            type: 'SECTION' as const,
            name: 'UNKNOWN-SECTION',
            children: [],
            paragraphs: [],
            sourceLines: [],
            performedBy: [],
            performs: [],
          }
        ],
        paragraphs: [
          {
            type: 'PARAGRAPH' as const,
            name: 'UNKNOWN-PARAGRAPH',
            children: [],
            sourceLines: [],
            performedBy: [],
            performs: [],
          }
        ],
        sourceLines: [createProcessedLine('PROCEDURE DIVISION.', 1)],
      };

      const errors = parser.validateStructure(procedureDivision);

      expect(errors).toContain("Section found with invalid name: UNKNOWN-SECTION");
      expect(errors).toContain("Section 'UNKNOWN-SECTION' has no source lines");
      expect(errors).toContain("Paragraph found with invalid name: UNKNOWN-PARAGRAPH");
      expect(errors).toContain("Paragraph 'UNKNOWN-PARAGRAPH' has no source lines");
    });

    it('should report errors for duplicate names', () => {
      const procedureDivision = {
        type: 'PROCEDURE_DIVISION' as const,
        children: [],
        sections: [
          {
            type: 'SECTION' as const,
            name: 'DUPLICATE-SECTION',
            children: [],
            paragraphs: [],
            sourceLines: [createProcessedLine('DUPLICATE-SECTION SECTION.', 1)],
            performedBy: [],
            performs: [],
          },
          {
            type: 'SECTION' as const,
            name: 'DUPLICATE-SECTION',
            children: [],
            paragraphs: [],
            sourceLines: [createProcessedLine('DUPLICATE-SECTION SECTION.', 2)],
            performedBy: [],
            performs: [],
          }
        ],
        paragraphs: [
          {
            type: 'PARAGRAPH' as const,
            name: 'DUPLICATE-PARA',
            children: [],
            sourceLines: [createProcessedLine('DUPLICATE-PARA.', 1)],
            performedBy: [],
            performs: [],
          },
          {
            type: 'PARAGRAPH' as const,
            name: 'DUPLICATE-PARA',
            children: [],
            sourceLines: [createProcessedLine('DUPLICATE-PARA.', 2)],
            performedBy: [],
            performs: [],
          }
        ],
        sourceLines: [createProcessedLine('PROCEDURE DIVISION.', 1)],
      };

      const errors = parser.validateStructure(procedureDivision);

      expect(errors).toContain('Duplicate section names found: DUPLICATE-SECTION');
      expect(errors).toContain('Duplicate paragraph names found: DUPLICATE-PARA');
    });
  });

  describe('edge cases', () => {
    it('should handle malformed section declarations', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('INVALID SECTION', 2), // Missing period
        createProcessedLine('    DISPLAY "Test".', 3),
        createProcessedLine('VALID-SECTION SECTION.', 4),
        createProcessedLine('    DISPLAY "Valid".', 5),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(1);
      expect(result!.sections[0].name).toBe('VALID-SECTION');
      expect(result!.paragraphs).toHaveLength(0);
    });

    it('should handle malformed paragraph declarations', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('INVALID PARA', 2), // Missing period
        createProcessedLine('    DISPLAY "Test".', 3),
        createProcessedLine('VALID-PARA.', 4),
        createProcessedLine('    DISPLAY "Valid".', 5),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.paragraphs).toHaveLength(1);
      expect(result!.paragraphs[0].name).toBe('VALID-PARA');
    });

    it('should handle empty PROCEDURE DIVISION', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(0);
      expect(result!.paragraphs).toHaveLength(0);
      expect(result!.sourceLines).toHaveLength(1);
    });

    it('should handle consecutive sections without content', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('FIRST-SECTION SECTION.', 2),
        createProcessedLine('SECOND-SECTION SECTION.', 3),
        createProcessedLine('THIRD-SECTION SECTION.', 4),
        createProcessedLine('    DISPLAY "Only third has content".', 5),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      expect(result!.sections).toHaveLength(3);
      expect(result!.sections[0].name).toBe('FIRST-SECTION');
      expect(result!.sections[1].name).toBe('SECOND-SECTION');
      expect(result!.sections[2].name).toBe('THIRD-SECTION');
      expect(result!.sections[2].sourceLines).toHaveLength(2); // Declaration + content
    });
  });

  describe('PERFORM statement analysis integration', () => {
    it('should analyze PERFORM statements in parsed procedures', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('MAIN-PARA.', 2),
        createProcessedLine('    PERFORM SUB-PARA.', 3),
        createProcessedLine('    PERFORM SECTION-1.', 4),
        createProcessedLine('SUB-PARA.', 5),
        createProcessedLine('    DISPLAY "Sub paragraph".', 6),
        createProcessedLine('SECTION-1 SECTION.', 7),
        createProcessedLine('SECTION-PARA.', 8),
        createProcessedLine('    PERFORM SUB-PARA THROUGH MAIN-PARA.', 9),
      ];

      const result = parser.parseProcedureDivision(lines);

      expect(result).not.toBeNull();
      
      // Check that PERFORM analysis was performed
      const mainPara = result!.paragraphs.find(p => p.name === 'MAIN-PARA');
      const subPara = result!.paragraphs.find(p => p.name === 'SUB-PARA');
      const section = result!.sections.find(s => s.name === 'SECTION-1');
      const sectionPara = section!.paragraphs.find(p => p.name === 'SECTION-PARA');

      expect(mainPara).toBeDefined();
      expect(subPara).toBeDefined();
      expect(section).toBeDefined();
      expect(sectionPara).toBeDefined();

      // Main paragraph should have 2 PERFORM statements
      expect(mainPara!.performs).toHaveLength(2);
      expect(mainPara!.performs[0].targetName).toBe('SUB-PARA');
      expect(mainPara!.performs[0].performType).toBe('SIMPLE');
      expect(mainPara!.performs[1].targetName).toBe('SECTION-1');
      expect(mainPara!.performs[1].performType).toBe('SIMPLE');

      // Section paragraph should have 1 THROUGH PERFORM
      expect(sectionPara!.performs).toHaveLength(1);
      expect(sectionPara!.performs[0].targetName).toBe('SUB-PARA');
      expect(sectionPara!.performs[0].performType).toBe('THROUGH');
      expect(sectionPara!.performs[0].throughTarget).toBe('MAIN-PARA');

      // Sub paragraph should be performed by 2 procedures
      // Note: THROUGH performs add references to both start and end targets
      expect(subPara!.performedBy).toHaveLength(2); // MAIN-PARA calls it, SECTION-PARA calls it through
      expect(mainPara!.performedBy).toHaveLength(1); // SECTION-PARA calls it through  
      expect(section!.performedBy).toHaveLength(1);
    });

    it('should validate PERFORM targets and report errors', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('MAIN-PARA.', 2),
        createProcessedLine('    PERFORM NONEXISTENT-PARA.', 3),
        createProcessedLine('    PERFORM VALID-PARA THROUGH INVALID-PARA.', 4),
        createProcessedLine('VALID-PARA.', 5),
        createProcessedLine('    DISPLAY "Valid".', 6),
      ];

      const result = parser.parseProcedureDivision(lines);
      expect(result).not.toBeNull();

      const errors = parser.validatePerformTargets(result!);
      expect(errors).toHaveLength(2);
      expect(errors[0]).toContain('NONEXISTENT-PARA');
      expect(errors[1]).toContain('INVALID-PARA');
      expect(errors[1]).toContain('THROUGH target');
    });

    it('should generate call graph report', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('MAIN-PARA.', 2),
        createProcessedLine('    PERFORM SUB-PARA.', 3),
        createProcessedLine('SUB-PARA.', 4),
        createProcessedLine('    DISPLAY "Sub".', 5),
      ];

      const result = parser.parseProcedureDivision(lines);
      expect(result).not.toBeNull();

      const report = parser.generateCallGraphReport(result!);
      expect(report).toContain('PROCEDURE CALL GRAPH');
      expect(report).toContain('MAIN-PARA');
      expect(report).toContain('SUB-PARA');
      expect(report).toContain('Calls:');
      expect(report).toContain('Called by:');
    });

    it('should provide access to symbol table', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('PARA-1.', 2),
        createProcessedLine('    DISPLAY "Para 1".', 3),
        createProcessedLine('SECTION-1 SECTION.', 4),
        createProcessedLine('PARA-2.', 5),
        createProcessedLine('    DISPLAY "Para 2".', 6),
      ];

      const result = parser.parseProcedureDivision(lines);
      expect(result).not.toBeNull();

      const symbolTable = parser.getSymbolTable();
      expect(symbolTable.size).toBe(3); // PARA-1, SECTION-1, PARA-2
      expect(symbolTable.has('PARA-1')).toBe(true);
      expect(symbolTable.has('SECTION-1')).toBe(true);
      expect(symbolTable.has('PARA-2')).toBe(true);
    });
  });

  describe('Section-level PERFORM statements', () => {
    it('should detect PERFORM statements directly in section code', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('', 2, 'BLANK'),
        createProcessedLine('FIRSTSECTION        SECTION.', 3),
        createProcessedLine('           IF  CONTINUA', 4),
        createProcessedLine('               MOVE DATA1 (IND-MOD IND-SOM)', 5),
        createProcessedLine('                    TO WS-CODICE-1', 6),
        createProcessedLine('           END-IF.', 7),
        createProcessedLine('', 8, 'BLANK'),
        createProcessedLine('           PERFORM FIRSTPERFORM.', 9),
        createProcessedLine('', 10, 'BLANK'),
        createProcessedLine('SECONDSECTION       SECTION.', 11),
        createProcessedLine('           PERFORM HELPER-ROUTINE.', 12),
        createProcessedLine('           DISPLAY "Processing data".', 13),
        createProcessedLine('           PERFORM CLEANUP THROUGH FINALIZE.', 14),
        createProcessedLine('', 15, 'BLANK'),
        createProcessedLine('THIRDPERFORM.', 16),
        createProcessedLine('           DISPLAY "Third perform executed".', 17),
        createProcessedLine('', 18, 'BLANK'),
        createProcessedLine('FIRSTPERFORM.', 19),
        createProcessedLine('           DISPLAY "First perform executed".', 20),
        createProcessedLine('', 21, 'BLANK'),
        createProcessedLine('HELPER-ROUTINE.', 22),
        createProcessedLine('           DISPLAY "Helper routine executed".', 23),
        createProcessedLine('', 24, 'BLANK'),
        createProcessedLine('CLEANUP.', 25),
        createProcessedLine('           DISPLAY "Cleanup started".', 26),
        createProcessedLine('', 27, 'BLANK'),
        createProcessedLine('FINALIZE.', 28),
        createProcessedLine('           DISPLAY "Finalize completed".', 29),
      ];

      const procedureDivision = parser.parseProcedureDivision(lines);

      expect(procedureDivision).toBeTruthy();
      expect(procedureDivision!.sections).toHaveLength(2);

      // Check FIRSTSECTION
      const firstSection = procedureDivision!.sections.find(s => s.name === 'FIRSTSECTION');
      expect(firstSection).toBeTruthy();
      expect(firstSection!.performs).toHaveLength(1);
      expect(firstSection!.performs[0].targetName).toBe('FIRSTPERFORM');
      expect(firstSection!.performs[0].performType).toBe('SIMPLE');

      // Check SECONDSECTION
      const secondSection = procedureDivision!.sections.find(s => s.name === 'SECONDSECTION');
      expect(secondSection).toBeTruthy();
      expect(secondSection!.performs).toHaveLength(2);
      
      // First PERFORM in SECONDSECTION
      expect(secondSection!.performs[0].targetName).toBe('HELPER-ROUTINE');
      expect(secondSection!.performs[0].performType).toBe('SIMPLE');
      
      // Second PERFORM (THROUGH) in SECONDSECTION
      expect(secondSection!.performs[1].targetName).toBe('CLEANUP');
      expect(secondSection!.performs[1].performType).toBe('THROUGH');
      expect(secondSection!.performs[1].throughTarget).toBe('FINALIZE');

      // Check bidirectional references
      const firstPerformPara = procedureDivision!.sections[1].paragraphs.find(p => p.name === 'FIRSTPERFORM');
      expect(firstPerformPara).toBeTruthy();
      expect(firstPerformPara!.performedBy).toHaveLength(1);
      expect(firstPerformPara!.performedBy[0].targetName).toBe('FIRSTSECTION');

      const helperRoutinePara = procedureDivision!.sections[1].paragraphs.find(p => p.name === 'HELPER-ROUTINE');
      expect(helperRoutinePara).toBeTruthy();
      expect(helperRoutinePara!.performedBy).toHaveLength(1);
      expect(helperRoutinePara!.performedBy[0].targetName).toBe('SECONDSECTION');

      const cleanupPara = procedureDivision!.sections[1].paragraphs.find(p => p.name === 'CLEANUP');
      expect(cleanupPara).toBeTruthy();
      expect(cleanupPara!.performedBy).toHaveLength(1);
      expect(cleanupPara!.performedBy[0].targetName).toBe('SECONDSECTION');
      expect(cleanupPara!.performedBy[0].performType).toBe('THROUGH');
    });

    it('should handle sections with both section-level and paragraph-level PERFORM statements', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('', 2, 'BLANK'),
        createProcessedLine('MAINSECTION         SECTION.', 3),
        createProcessedLine('           PERFORM INIT-ROUTINE.', 4),
        createProcessedLine('           DISPLAY "Section-level code".', 5),
        createProcessedLine('', 6, 'BLANK'),
        createProcessedLine('MAIN-PARA.', 7),
        createProcessedLine('           PERFORM SUB-ROUTINE.', 8),
        createProcessedLine('           DISPLAY "Paragraph-level code".', 9),
        createProcessedLine('', 10, 'BLANK'),
        createProcessedLine('ANOTHERSECTION      SECTION.', 11),
        createProcessedLine('', 12, 'BLANK'),
        createProcessedLine('ANOTHER-PARA.', 13),
        createProcessedLine('           PERFORM FINAL-ROUTINE.', 14),
        createProcessedLine('', 15, 'BLANK'),
        createProcessedLine('INIT-ROUTINE.', 16),
        createProcessedLine('           DISPLAY "Initialization".', 17),
        createProcessedLine('', 18, 'BLANK'),
        createProcessedLine('SUB-ROUTINE.', 19),
        createProcessedLine('           DISPLAY "Sub routine".', 20),
        createProcessedLine('', 21, 'BLANK'),
        createProcessedLine('FINAL-ROUTINE.', 22),
        createProcessedLine('           DISPLAY "Final routine".', 23),
      ];

      const procedureDivision = parser.parseProcedureDivision(lines);

      expect(procedureDivision).toBeTruthy();

      // Check MAINSECTION has section-level PERFORM
      const mainSection = procedureDivision!.sections.find(s => s.name === 'MAINSECTION');
      expect(mainSection).toBeTruthy();
      expect(mainSection!.performs).toHaveLength(1);
      expect(mainSection!.performs[0].targetName).toBe('INIT-ROUTINE');

      // Check MAIN-PARA has paragraph-level PERFORM
      const mainPara = mainSection!.paragraphs.find(p => p.name === 'MAIN-PARA');
      expect(mainPara).toBeTruthy();
      expect(mainPara!.performs).toHaveLength(1);
      expect(mainPara!.performs[0].targetName).toBe('SUB-ROUTINE');

      // Check ANOTHERSECTION has no section-level PERFORM
      const anotherSection = procedureDivision!.sections.find(s => s.name === 'ANOTHERSECTION');
      expect(anotherSection).toBeTruthy();
      expect(anotherSection!.performs).toHaveLength(0);

      // Check ANOTHER-PARA has paragraph-level PERFORM
      const anotherPara = anotherSection!.paragraphs.find(p => p.name === 'ANOTHER-PARA');
      expect(anotherPara).toBeTruthy();
      expect(anotherPara!.performs).toHaveLength(1);
      expect(anotherPara!.performs[0].targetName).toBe('FINAL-ROUTINE');

      // Verify bidirectional references      
      const initRoutine = anotherSection!.paragraphs.find(p => p.name === 'INIT-ROUTINE');
      expect(initRoutine).toBeTruthy();
      expect(initRoutine!.performedBy).toHaveLength(1);
      expect(initRoutine!.performedBy[0].targetName).toBe('MAINSECTION');

      const subRoutine = anotherSection!.paragraphs.find(p => p.name === 'SUB-ROUTINE');
      expect(subRoutine).toBeTruthy();
      expect(subRoutine!.performedBy).toHaveLength(1);
      expect(subRoutine!.performedBy[0].targetName).toBe('MAIN-PARA');

      const finalRoutine = anotherSection!.paragraphs.find(p => p.name === 'FINAL-ROUTINE');
      expect(finalRoutine).toBeTruthy();
      expect(finalRoutine!.performedBy).toHaveLength(1);
      expect(finalRoutine!.performedBy[0].targetName).toBe('ANOTHER-PARA');
    });

    it('should handle complex section-level PERFORM patterns', () => {
      const lines: ProcessedLine[] = [
        createProcessedLine('PROCEDURE DIVISION.', 1),
        createProcessedLine('', 2, 'BLANK'),
        createProcessedLine('COMPLEXSECTION      SECTION.', 3),
        createProcessedLine('           IF DATA-FLAG = \'Y\'', 4),
        createProcessedLine('               PERFORM VALIDATE-DATA', 5),
        createProcessedLine('           END-IF.', 6),
        createProcessedLine('           ', 7),
        createProcessedLine('           PERFORM PROCESS-LOOP 5 TIMES.', 8),
        createProcessedLine('           ', 9),
        createProcessedLine('           PERFORM START-PROCESS THROUGH END-PROCESS.', 10),
        createProcessedLine('           ', 11),
        createProcessedLine('           DISPLAY "Section processing complete".', 12),
        createProcessedLine('', 13, 'BLANK'),
        createProcessedLine('VALIDATE-DATA.', 14),
        createProcessedLine('           DISPLAY "Data validation".', 15),
        createProcessedLine('', 16, 'BLANK'),
        createProcessedLine('PROCESS-LOOP.', 17),
        createProcessedLine('           DISPLAY "Loop iteration".', 18),
        createProcessedLine('', 19, 'BLANK'),
        createProcessedLine('START-PROCESS.', 20),
        createProcessedLine('           DISPLAY "Start processing".', 21),
        createProcessedLine('', 22, 'BLANK'),
        createProcessedLine('MIDDLE-STEP.', 23),
        createProcessedLine('           DISPLAY "Middle step".', 24),
        createProcessedLine('', 25, 'BLANK'),
        createProcessedLine('END-PROCESS.', 26),
        createProcessedLine('           DISPLAY "End processing".', 27),
      ];

      const procedureDivision = parser.parseProcedureDivision(lines);

      expect(procedureDivision).toBeTruthy();

      const complexSection = procedureDivision!.sections.find(s => s.name === 'COMPLEXSECTION');
      expect(complexSection).toBeTruthy();
      expect(complexSection!.performs).toHaveLength(3);

      // Check different PERFORM types in section
      const performs = complexSection!.performs;
      
      // Simple PERFORM
      const simplePerform = performs.find(p => p.targetName === 'VALIDATE-DATA');
      expect(simplePerform).toBeTruthy();
      expect(simplePerform!.performType).toBe('SIMPLE');

      // TIMES PERFORM
      const timesPerform = performs.find(p => p.targetName === 'PROCESS-LOOP');
      expect(timesPerform).toBeTruthy();
      expect(timesPerform!.performType).toBe('TIMES');

      // THROUGH PERFORM
      const throughPerform = performs.find(p => p.targetName === 'START-PROCESS');
      expect(throughPerform).toBeTruthy();
      expect(throughPerform!.performType).toBe('THROUGH');
      expect(throughPerform!.throughTarget).toBe('END-PROCESS');
    });
  });
});
