import { Logger } from '../src/utils/logger';
import { PerformAnalyzer } from '../src/parser/perform-analyzer';
import { 
  ProcessedLine, 
  ProcedureDivisionNode, 
  SectionNode, 
  ParagraphNode
} from '../src/types';

describe('PerformAnalyzer', () => {
  let analyzer: PerformAnalyzer;
  let mockLogger: jest.Mocked<Logger>;

  beforeEach(() => {
    mockLogger = new Logger() as jest.Mocked<Logger>;
    jest.spyOn(mockLogger, 'info');
    jest.spyOn(mockLogger, 'warn');
    jest.spyOn(mockLogger, 'error');
    jest.spyOn(mockLogger, 'verbose');
    jest.spyOn(mockLogger, 'success');
    jest.spyOn(mockLogger, 'setVerbose');
    
    analyzer = new PerformAnalyzer(mockLogger);
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

  const createParagraph = (name: string, lines: ProcessedLine[]): ParagraphNode => ({
    type: 'PARAGRAPH',
    name,
    children: [],
    sourceLines: lines,
    performedBy: [],
    performs: [],
  });

  const createSection = (name: string, lines: ProcessedLine[], paragraphs: ParagraphNode[] = []): SectionNode => ({
    type: 'SECTION',
    name,
    children: paragraphs,
    paragraphs,
    sourceLines: lines,
    performedBy: [],
    performs: [],
  });

  const createProcedureDivision = (sections: SectionNode[], paragraphs: ParagraphNode[]): ProcedureDivisionNode => ({
    type: 'PROCEDURE_DIVISION',
    children: [...sections, ...paragraphs],
    sections,
    paragraphs,
    sourceLines: [],
  });

  describe('Simple PERFORM statements', () => {
    it('should detect simple PERFORM statements', () => {
      const lines = [
        createProcessedLine('PERFORM TARGET-PARA.', 1),
        createProcessedLine('PERFORM ANOTHER-PARA', 2),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const targetPara = createParagraph('TARGET-PARA', []);
      const anotherPara = createParagraph('ANOTHER-PARA', []);
      
      const procedureDivision = createProcedureDivision([], [callerPara, targetPara, anotherPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(2);
      expect(callerPara.performs[0].targetName).toBe('TARGET-PARA');
      expect(callerPara.performs[0].performType).toBe('SIMPLE');
      expect(callerPara.performs[1].targetName).toBe('ANOTHER-PARA');
      expect(callerPara.performs[1].performType).toBe('SIMPLE');
      
      expect(targetPara.performedBy).toHaveLength(1);
      expect(targetPara.performedBy[0].targetName).toBe('CALLER-PARA');
      expect(anotherPara.performedBy).toHaveLength(1);
      expect(anotherPara.performedBy[0].targetName).toBe('CALLER-PARA');
    });

    it('should ignore non-PERFORM statements', () => {
      const lines = [
        createProcessedLine('DISPLAY "Hello"', 1),
        createProcessedLine('MOVE X TO Y', 2),
        createProcessedLine('IF CONDITION', 3),
      ];
      
      const para = createParagraph('TEST-PARA', lines);
      const procedureDivision = createProcedureDivision([], [para]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(para.performs).toHaveLength(0);
    });
  });

  describe('PERFORM THROUGH statements', () => {
    it('should detect PERFORM THROUGH statements', () => {
      const lines = [
        createProcessedLine('PERFORM START-PARA THROUGH END-PARA.', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const startPara = createParagraph('START-PARA', []);
      const middlePara = createParagraph('MIDDLE-PARA', []);
      const endPara = createParagraph('END-PARA', []);
      
      const procedureDivision = createProcedureDivision([], [callerPara, startPara, middlePara, endPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(1);
      expect(callerPara.performs[0].targetName).toBe('START-PARA');
      expect(callerPara.performs[0].performType).toBe('THROUGH');
      expect(callerPara.performs[0].throughTarget).toBe('END-PARA');
      
      expect(startPara.performedBy).toHaveLength(1);
      expect(endPara.performedBy).toHaveLength(1);
    });
  });

  describe('Loop PERFORM statements', () => {
    it('should detect PERFORM TIMES statements', () => {
      const lines = [
        createProcessedLine('PERFORM LOOP-PARA 5 TIMES.', 1),
        createProcessedLine('PERFORM LOOP-PARA COUNTER TIMES.', 2),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const loopPara = createParagraph('LOOP-PARA', []);
      
      const procedureDivision = createProcedureDivision([], [callerPara, loopPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(2);
      expect(callerPara.performs[0].targetName).toBe('LOOP-PARA');
      expect(callerPara.performs[0].performType).toBe('TIMES');
      expect(callerPara.performs[1].targetName).toBe('LOOP-PARA');
      expect(callerPara.performs[1].performType).toBe('TIMES');
      
      expect(loopPara.performedBy).toHaveLength(2);
    });

    it('should detect PERFORM UNTIL statements', () => {
      const lines = [
        createProcessedLine('PERFORM LOOP-PARA UNTIL FLAG = "Y".', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const loopPara = createParagraph('LOOP-PARA', []);
      
      const procedureDivision = createProcedureDivision([], [callerPara, loopPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(1);
      expect(callerPara.performs[0].targetName).toBe('LOOP-PARA');
      expect(callerPara.performs[0].performType).toBe('UNTIL');
      
      expect(loopPara.performedBy).toHaveLength(1);
    });

    it('should detect PERFORM VARYING statements', () => {
      const lines = [
        createProcessedLine('PERFORM LOOP-PARA VARYING I FROM 1 BY 1 UNTIL I > 10.', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const loopPara = createParagraph('LOOP-PARA', []);
      
      const procedureDivision = createProcedureDivision([], [callerPara, loopPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(1);
      expect(callerPara.performs[0].targetName).toBe('LOOP-PARA');
      expect(callerPara.performs[0].performType).toBe('VARYING');
      
      expect(loopPara.performedBy).toHaveLength(1);
    });
  });

  describe('Section and paragraph mixed scenarios', () => {
    it('should handle PERFORM statements targeting sections', () => {
      const lines = [
        createProcessedLine('PERFORM MAIN-SECTION.', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const targetSection = createSection('MAIN-SECTION', []);
      
      const procedureDivision = createProcedureDivision([targetSection], [callerPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(1);
      expect(callerPara.performs[0].targetName).toBe('MAIN-SECTION');
      
      expect(targetSection.performedBy).toHaveLength(1);
      expect(targetSection.performedBy[0].targetName).toBe('CALLER-PARA');
    });

    it('should handle PERFORM statements within sections', () => {
      const sectionLines = [
        createProcessedLine('PERFORM TARGET-PARA.', 1),
      ];
      
      const sectionPara = createParagraph('SECTION-PARA', sectionLines);
      const targetPara = createParagraph('TARGET-PARA', []);
      const section = createSection('MAIN-SECTION', [], [sectionPara]);
      
      const procedureDivision = createProcedureDivision([section], [targetPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(sectionPara.performs).toHaveLength(1);
      expect(sectionPara.performs[0].targetName).toBe('TARGET-PARA');
      
      expect(targetPara.performedBy).toHaveLength(1);
      expect(targetPara.performedBy[0].targetName).toBe('SECTION-PARA');
    });
  });

  describe('Error handling and validation', () => {
    it('should report unresolved PERFORM targets', () => {
      const lines = [
        createProcessedLine('PERFORM NONEXISTENT-PARA.', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const procedureDivision = createProcedureDivision([], [callerPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      const errors = analyzer.validatePerformTargets(procedureDivision);
      expect(errors).toHaveLength(1);
      expect(errors[0]).toContain('NONEXISTENT-PARA');
      expect(errors[0]).toContain('not found');
      expect(mockLogger.warn).toHaveBeenCalledWith(
        expect.stringContaining('NONEXISTENT-PARA')
      );
    });

    it('should report unresolved THROUGH targets', () => {
      const lines = [
        createProcessedLine('PERFORM START-PARA THROUGH NONEXISTENT-PARA.', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const startPara = createParagraph('START-PARA', []);
      const procedureDivision = createProcedureDivision([], [callerPara, startPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      const errors = analyzer.validatePerformTargets(procedureDivision);
      expect(errors).toHaveLength(1);
      expect(errors[0]).toContain('NONEXISTENT-PARA');
      expect(errors[0]).toContain('THROUGH target');
    });

    it('should handle case-insensitive procedure names', () => {
      const lines = [
        createProcessedLine('PERFORM target-para.', 1),
      ];
      
      const callerPara = createParagraph('CALLER-PARA', lines);
      const targetPara = createParagraph('TARGET-PARA', []); // uppercase
      
      const procedureDivision = createProcedureDivision([], [callerPara, targetPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      expect(callerPara.performs).toHaveLength(1);
      expect(callerPara.performs[0].targetName).toBe('TARGET-PARA'); // normalized to uppercase
      expect(targetPara.performedBy).toHaveLength(1);
    });
  });

  describe('Symbol table functionality', () => {
    it('should build correct symbol table', () => {
      const para1 = createParagraph('PARA-1', []);
      const para2 = createParagraph('PARA-2', []);
      const section = createSection('MAIN-SECTION', [], [para2]);
      
      const procedureDivision = createProcedureDivision([section], [para1]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      const symbolTable = analyzer.getSymbolTable();
      expect(symbolTable.size).toBe(3);
      expect(symbolTable.has('PARA-1')).toBe(true);
      expect(symbolTable.has('PARA-2')).toBe(true);
      expect(symbolTable.has('MAIN-SECTION')).toBe(true);
    });
  });

  describe('Call graph report generation', () => {
    it('should generate correct call graph report', () => {
      const callerLines = [createProcessedLine('PERFORM TARGET-PARA.', 1)];
      const callerPara = createParagraph('CALLER-PARA', callerLines);
      const targetPara = createParagraph('TARGET-PARA', []);
      
      const procedureDivision = createProcedureDivision([], [callerPara, targetPara]);
      
      analyzer.analyzePerformStatements(procedureDivision);
      
      const report = analyzer.generateCallGraphReport(procedureDivision);
      
      expect(report).toContain('PROCEDURE CALL GRAPH');
      expect(report).toContain('CALLER-PARA');
      expect(report).toContain('TARGET-PARA');
      expect(report).toContain('Calls:');
      expect(report).toContain('Called by:');
      expect(report).toContain('SIMPLE');
    });
  });
});
