import { CobolUtils } from '../src/utils/cobol-utils';
import { ProcessedLine } from '../src/types';

describe('CobolUtils', () => {
  const createTestLine = (
    content: string,
    lineNumber: number = 1,
    sourceFile: string = 'test.cbl'
  ): ProcessedLine => ({
    content,
    originalLineNumber: lineNumber,
    sourceFile,
    isFromCopy: false,
    lineType: 'CODE',
    rawContent: content,
  });

  describe('normalizeIdentifier', () => {
    it('should convert to uppercase and trim', () => {
      expect(CobolUtils.normalizeIdentifier('  hello-world  ')).toBe('HELLO-WORLD');
      expect(CobolUtils.normalizeIdentifier('test_name')).toBe('TEST_NAME');
      expect(CobolUtils.normalizeIdentifier('MyVariable')).toBe('MYVARIABLE');
    });
  });

  describe('isValidIdentifier', () => {
    it('should accept valid COBOL identifiers', () => {
      expect(CobolUtils.isValidIdentifier('HELLO-WORLD')).toBe(true);
      expect(CobolUtils.isValidIdentifier('TEST123')).toBe(true);
      expect(CobolUtils.isValidIdentifier('A')).toBe(true);
      expect(CobolUtils.isValidIdentifier('MY-VAR-123')).toBe(true);
    });

    it('should reject invalid COBOL identifiers', () => {
      expect(CobolUtils.isValidIdentifier('123TEST')).toBe(false); // Starts with number
      expect(CobolUtils.isValidIdentifier('HELLO-')).toBe(false); // Ends with hyphen
      expect(CobolUtils.isValidIdentifier('-HELLO')).toBe(false); // Starts with hyphen
      expect(CobolUtils.isValidIdentifier('')).toBe(false); // Empty
      expect(CobolUtils.isValidIdentifier('HELLO WORLD')).toBe(false); // Contains space
    });
  });

  describe('extractWords', () => {
    it('should split on whitespace and punctuation', () => {
      expect(CobolUtils.extractWords('HELLO WORLD')).toEqual(['HELLO', 'WORLD']);
      expect(CobolUtils.extractWords('PERFORM PARA-1.')).toEqual(['PERFORM', 'PARA-1']);
      expect(CobolUtils.extractWords('IF A = B THEN')).toEqual(['IF', 'A', '=', 'B', 'THEN']);
      expect(CobolUtils.extractWords('DISPLAY "HELLO";')).toEqual(['DISPLAY', '"HELLO"']);
    });

    it('should handle empty or whitespace-only strings', () => {
      expect(CobolUtils.extractWords('')).toEqual([]);
      expect(CobolUtils.extractWords('   ')).toEqual([]);
      expect(CobolUtils.extractWords('.,;()')).toEqual([]);
    });
  });

  describe('isDivisionHeader', () => {
    it('should identify division headers', () => {
      expect(CobolUtils.isDivisionHeader(createTestLine('IDENTIFICATION DIVISION.'))).toBe(true);
      expect(CobolUtils.isDivisionHeader(createTestLine('ENVIRONMENT DIVISION.'))).toBe(true);
      expect(CobolUtils.isDivisionHeader(createTestLine('DATA DIVISION.'))).toBe(true);
      expect(CobolUtils.isDivisionHeader(createTestLine('PROCEDURE DIVISION.'))).toBe(true);
      expect(CobolUtils.isDivisionHeader(createTestLine('procedure division.'))).toBe(true); // Case insensitive
    });

    it('should not identify non-division headers', () => {
      expect(CobolUtils.isDivisionHeader(createTestLine('MAIN-SECTION SECTION.'))).toBe(false);
      expect(CobolUtils.isDivisionHeader(createTestLine('PARA-1.'))).toBe(false);
      expect(CobolUtils.isDivisionHeader(createTestLine('DISPLAY "HELLO".'))).toBe(false);
    });
  });

  describe('isSectionHeader', () => {
    it('should identify section headers', () => {
      expect(CobolUtils.isSectionHeader(createTestLine('MAIN-SECTION SECTION.'))).toBe(true);
      expect(CobolUtils.isSectionHeader(createTestLine('WORKING-STORAGE SECTION'))).toBe(true);
      expect(CobolUtils.isSectionHeader(createTestLine('FILE SECTION.'))).toBe(true);
    });

    it('should not identify non-section headers', () => {
      expect(CobolUtils.isSectionHeader(createTestLine('PROCEDURE DIVISION.'))).toBe(false);
      expect(CobolUtils.isSectionHeader(createTestLine('PARA-1.'))).toBe(false);
      expect(CobolUtils.isSectionHeader(createTestLine('DISPLAY SECTION-VAR.'))).toBe(false);
    });
  });

  describe('extractSectionName', () => {
    it('should extract section names correctly', () => {
      expect(CobolUtils.extractSectionName(createTestLine('MAIN-SECTION SECTION.'))).toBe('MAIN-SECTION');
      expect(CobolUtils.extractSectionName(createTestLine('WORKING-STORAGE SECTION'))).toBe('WORKING-STORAGE');
      expect(CobolUtils.extractSectionName(createTestLine('FILE SECTION.'))).toBe('FILE');
    });

    it('should return null for non-section headers', () => {
      expect(CobolUtils.extractSectionName(createTestLine('PARA-1.'))).toBeNull();
      expect(CobolUtils.extractSectionName(createTestLine('DISPLAY "HELLO".'))).toBeNull();
    });
  });

  describe('isParagraphHeader', () => {
    it('should identify paragraph headers', () => {
      expect(CobolUtils.isParagraphHeader(createTestLine('PARA-1.'))).toBe(true);
      expect(CobolUtils.isParagraphHeader(createTestLine('MAIN-ROUTINE.'))).toBe(true);
      expect(CobolUtils.isParagraphHeader(createTestLine('EXIT-ROUTINE.'))).toBe(true);
    });

    it('should not identify statements as paragraph headers', () => {
      expect(CobolUtils.isParagraphHeader(createTestLine('DISPLAY "HELLO".'))).toBe(false);
      expect(CobolUtils.isParagraphHeader(createTestLine('PERFORM PARA-1.'))).toBe(false);
      expect(CobolUtils.isParagraphHeader(createTestLine('MOVE ZERO TO COUNTER.'))).toBe(false);
    });

    it('should not identify multi-word lines as paragraph headers', () => {
      expect(CobolUtils.isParagraphHeader(createTestLine('MAIN SECTION.'))).toBe(false);
      expect(CobolUtils.isParagraphHeader(createTestLine('IF CONDITION THEN.'))).toBe(false);
    });

    it('should handle lines without periods', () => {
      expect(CobolUtils.isParagraphHeader(createTestLine('PARA-1'))).toBe(false);
      expect(CobolUtils.isParagraphHeader(createTestLine('MAIN-ROUTINE'))).toBe(false);
    });
  });

  describe('extractParagraphName', () => {
    it('should extract paragraph names correctly', () => {
      expect(CobolUtils.extractParagraphName(createTestLine('PARA-1.'))).toBe('PARA-1');
      expect(CobolUtils.extractParagraphName(createTestLine('MAIN-ROUTINE.'))).toBe('MAIN-ROUTINE');
      expect(CobolUtils.extractParagraphName(createTestLine('exit-routine.'))).toBe('EXIT-ROUTINE');
    });

    it('should return null for non-paragraph headers', () => {
      expect(CobolUtils.extractParagraphName(createTestLine('DISPLAY "HELLO".'))).toBeNull();
      expect(CobolUtils.extractParagraphName(createTestLine('PERFORM PARA-1.'))).toBeNull();
    });
  });

  describe('isPerformStatement', () => {
    it('should identify PERFORM statements', () => {
      expect(CobolUtils.isPerformStatement(createTestLine('PERFORM PARA-1.'))).toBe(true);
      expect(CobolUtils.isPerformStatement(createTestLine('PERFORM PARA-1 THROUGH PARA-3.'))).toBe(true);
      expect(CobolUtils.isPerformStatement(createTestLine('perform routine-1.'))).toBe(true); // Case insensitive
      expect(CobolUtils.isPerformStatement(createTestLine('PERFORM'))).toBe(true);
    });

    it('should not identify non-PERFORM statements', () => {
      expect(CobolUtils.isPerformStatement(createTestLine('DISPLAY "HELLO".'))).toBe(false);
      expect(CobolUtils.isPerformStatement(createTestLine('MOVE ZERO TO COUNTER.'))).toBe(false);
      expect(CobolUtils.isPerformStatement(createTestLine('CALL PERFORM-SUB.'))).toBe(false); // Contains PERFORM but doesn't start with it
    });
  });

  describe('isCobolStatement', () => {
    it('should identify COBOL statements', () => {
      expect(CobolUtils.isCobolStatement('DISPLAY')).toBe(true);
      expect(CobolUtils.isCobolStatement('MOVE ZERO TO VAR')).toBe(true);
      expect(CobolUtils.isCobolStatement('PERFORM PARA-1')).toBe(true);
      expect(CobolUtils.isCobolStatement('IF CONDITION')).toBe(true);
      expect(CobolUtils.isCobolStatement('CALL "PROGRAM"')).toBe(true);
    });

    it('should not identify non-statements', () => {
      expect(CobolUtils.isCobolStatement('PARA-1')).toBe(false);
      expect(CobolUtils.isCobolStatement('MAIN-SECTION')).toBe(false);
      expect(CobolUtils.isCobolStatement('HELLO-WORLD')).toBe(false);
    });
  });

  describe('cleanContent', () => {
    it('should normalize whitespace and punctuation', () => {
      expect(CobolUtils.cleanContent('  HELLO    WORLD  ')).toBe('HELLO WORLD');
      expect(CobolUtils.cleanContent('DISPLAY"HELLO".MOVE')).toBe('DISPLAY"HELLO". MOVE');
      expect(CobolUtils.cleanContent('A,B,C')).toBe('A, B, C');
    });
  });

  describe('isEffectivelyEmpty', () => {
    it('should identify empty lines', () => {
      expect(CobolUtils.isEffectivelyEmpty(createTestLine(''))).toBe(true);
      expect(CobolUtils.isEffectivelyEmpty({ ...createTestLine(''), lineType: 'BLANK' })).toBe(true);
      expect(CobolUtils.isEffectivelyEmpty({ ...createTestLine('comment'), lineType: 'COMMENT' })).toBe(true);
      expect(CobolUtils.isEffectivelyEmpty(createTestLine('   '))).toBe(true);
    });

    it('should not identify content lines as empty', () => {
      expect(CobolUtils.isEffectivelyEmpty(createTestLine('DISPLAY "HELLO".'))).toBe(false);
      expect(CobolUtils.isEffectivelyEmpty(createTestLine('PARA-1.'))).toBe(false);
    });
  });

  describe('extractQuotedStrings', () => {
    it('should extract quoted strings', () => {
      expect(CobolUtils.extractQuotedStrings('DISPLAY "HELLO WORLD".')).toEqual(['HELLO WORLD']);
      expect(CobolUtils.extractQuotedStrings('DISPLAY "A" AND "B".')).toEqual(['A', 'B']);
      expect(CobolUtils.extractQuotedStrings("DISPLAY 'SINGLE' AND \"DOUBLE\".")).toEqual(['SINGLE', 'DOUBLE']);
    });

    it('should handle strings with no quotes', () => {
      expect(CobolUtils.extractQuotedStrings('DISPLAY VARIABLE.')).toEqual([]);
    });

    it('should handle malformed quotes', () => {
      expect(CobolUtils.extractQuotedStrings('DISPLAY "UNCLOSED')).toEqual([]);
      expect(CobolUtils.extractQuotedStrings('DISPLAY UNCLOSED"')).toEqual([]);
    });
  });

  describe('removeQuotedStrings', () => {
    it('should replace quoted strings with placeholder', () => {
      expect(CobolUtils.removeQuotedStrings('DISPLAY "HELLO".')).toBe('DISPLAY STRING.');
      expect(CobolUtils.removeQuotedStrings('DISPLAY "A" AND "B".', 'X')).toBe('DISPLAY X AND X.');
    });
  });

  describe('splitStatements', () => {
    it('should split on periods outside of strings', () => {
      const result = CobolUtils.splitStatements('DISPLAY "HELLO". MOVE ZERO TO VAR.');
      expect(result).toEqual(['DISPLAY "HELLO"', 'MOVE ZERO TO VAR']);
    });

    it('should preserve periods inside strings', () => {
      const result = CobolUtils.splitStatements('DISPLAY "HELLO. WORLD". MOVE ZERO TO VAR.');
      expect(result).toEqual(['DISPLAY "HELLO. WORLD"', 'MOVE ZERO TO VAR']);
    });

    it('should handle single statement', () => {
      const result = CobolUtils.splitStatements('DISPLAY "HELLO"');
      expect(result).toEqual(['DISPLAY "HELLO"']);
    });
  });

  describe('getHierarchicalLevel', () => {
    it('should return correct hierarchical levels', () => {
      expect(CobolUtils.getHierarchicalLevel(createTestLine('PROCEDURE DIVISION.'))).toBe(0);
      expect(CobolUtils.getHierarchicalLevel(createTestLine('MAIN-SECTION SECTION.'))).toBe(1);
      expect(CobolUtils.getHierarchicalLevel(createTestLine('PARA-1.'))).toBe(2);
      expect(CobolUtils.getHierarchicalLevel(createTestLine('DISPLAY "HELLO".'))).toBe(3);
    });
  });

  describe('formatError', () => {
    it('should format error messages with context', () => {
      const line = createTestLine('INVALID LINE', 42, 'program.cbl');
      const result = CobolUtils.formatError('Syntax error', line, 'PARSER');
      
      expect(result).toBe('[PARSER] Syntax error at program.cbl:42: "INVALID LINE"');
    });

    it('should format error messages without context', () => {
      const line = createTestLine('INVALID LINE', 42, 'program.cbl');
      const result = CobolUtils.formatError('Syntax error', line);
      
      expect(result).toBe('Syntax error at program.cbl:42: "INVALID LINE"');
    });
  });
});
