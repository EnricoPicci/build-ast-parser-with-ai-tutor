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
});
