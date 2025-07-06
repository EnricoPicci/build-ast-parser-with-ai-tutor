import { ASTBuilder } from '../src/parser/ast-builder';
import { Logger } from '../src/utils/logger';
import * as fs from 'fs/promises';
import * as path from 'path';

describe('ASTBuilder', () => {
  let astBuilder: ASTBuilder;
  let logger: Logger;
  let tempDir: string;

  beforeEach(() => {
    logger = new Logger(false); // Disable verbose logging for tests
    astBuilder = new ASTBuilder(logger);
    tempDir = path.join(__dirname, 'temp');
  });

  afterEach(async () => {
    try {
      await fs.rmdir(tempDir, { recursive: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  describe('buildAST', () => {
    it('should build AST for simple COBOL program', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Hello World".
           PERFORM SUB-PARA.
           STOP RUN.
           
       SUB-PARA.
           DISPLAY "Sub paragraph".
           EXIT.`;

      const testFile = await createTempFile('simple.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();
      expect(result.ast!.type).toBe('PROGRAM');
      expect(result.ast!.name).toBe('SIMPLE-PROGRAM');
      expect(result.ast!.procedureDivision).toBeDefined();
      expect(result.ast!.procedureDivision!.paragraphs).toHaveLength(2);
      expect(result.ast!.procedureDivision!.paragraphs[0].name).toBe('MAIN-PARA');
      expect(result.ast!.procedureDivision!.paragraphs[1].name).toBe('SUB-PARA');
    });

    it('should handle COBOL program with sections', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECTION-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-SECTION SECTION.
       MAIN-PARA.
           DISPLAY "Main section".
           PERFORM SUB-SECTION.
           STOP RUN.
           
       SUB-SECTION SECTION.
       SUB-PARA.
           DISPLAY "Sub section".
           EXIT.`;

      const testFile = await createTempFile('sections.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();
      expect(result.ast!.procedureDivision!.sections).toHaveLength(2);
      expect(result.ast!.procedureDivision!.sections[0].name).toBe('MAIN-SECTION');
      expect(result.ast!.procedureDivision!.sections[1].name).toBe('SUB-SECTION');
    });

    it('should detect validation errors for missing PERFORM targets', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. ERROR-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Main".
           PERFORM MISSING-PARA.
           STOP RUN.`;

      const testFile = await createTempFile('error.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.validationIssues).toBeDefined();
      expect(result.validationIssues!.length).toBeGreaterThan(0);
      
      const errorIssues = result.validationIssues!.filter(issue => issue.type === 'ERROR');
      expect(errorIssues).toHaveLength(1);
      expect(errorIssues[0].message).toContain('PERFORM target not found: MISSING-PARA');
    });

    it('should handle programs without PROCEDURE DIVISION', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. NO-PROC-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X(10).`;

      const testFile = await createTempFile('no-proc.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();
      expect(result.ast!.procedureDivision).toBeUndefined();
      expect(result.validationIssues).toBeDefined();
      
      const warningIssues = result.validationIssues!.filter(issue => issue.type === 'WARNING');
      expect(warningIssues.some(issue => issue.message.includes('No PROCEDURE DIVISION found'))).toBe(true);
    });

    it('should calculate correct statistics', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATS-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-SECTION SECTION.
       PARA-1.
           DISPLAY "Para 1".
           PERFORM PARA-2.
           PERFORM PARA-3.
           
       PARA-2.
           DISPLAY "Para 2".
           
       PARA-3.
           DISPLAY "Para 3".
           STOP RUN.`;

      const testFile = await createTempFile('stats.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.stats).toBeDefined();
      expect(result.stats!.sectionCount).toBe(1);
      expect(result.stats!.paragraphCount).toBe(3);
      expect(result.stats!.performCount).toBe(2);
      expect(result.stats!.processingTime).toBeGreaterThan(0);
    });
  });

  describe('serializeToJSON', () => {
    it('should serialize AST to well-formed JSON', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "JSON test".
           STOP RUN.`;

      const testFile = await createTempFile('json.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();

      const jsonString = astBuilder.serializeToJSON(result.ast!, true);
      expect(() => JSON.parse(jsonString)).not.toThrow();
      
      const parsedAST = JSON.parse(jsonString);
      expect(parsedAST.type).toBe('PROGRAM');
      expect(parsedAST.name).toBe('JSON-PROGRAM');
    });

    it('should maintain consistent property ordering', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Order test".
           STOP RUN.`;

      const testFile = await createTempFile('order.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();

      const jsonString = astBuilder.serializeToJSON(result.ast!, true);
      const parsedAST = JSON.parse(jsonString);
      
      // Check that type comes first
      const keys = Object.keys(parsedAST);
      expect(keys[0]).toBe('type');
      expect(keys[1]).toBe('name');
    });
  });

  describe('generateOutputFilename', () => {
    it('should generate correct output filename', () => {
      const inputPath = '/path/to/program.cbl';
      const outputPath = astBuilder.generateOutputFilename(inputPath);
      expect(outputPath).toBe('/path/to/program.ast.json');
    });

    it('should handle Windows paths', () => {
      const inputPath = 'C:\\path\\to\\program.PCO';
      const outputPath = astBuilder.generateOutputFilename(inputPath);
      expect(outputPath).toBe('C:/path/to/program.ast.json');
    });

    it('should handle filenames without extensions', () => {
      const inputPath = '/path/to/program';
      const outputPath = astBuilder.generateOutputFilename(inputPath);
      expect(outputPath).toBe('/path/to/program.ast.json');
    });
  });

  describe('saveASTToFile', () => {
    it('should save AST to file with metadata', async () => {
      const cobolContent = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAVE-PROGRAM.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Save test".
           STOP RUN.`;

      const testFile = await createTempFile('save.cbl', cobolContent);
      const result = await astBuilder.buildAST(testFile);

      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();

      const outputFile = path.join(tempDir, 'output.ast.json');
      await astBuilder.saveASTToFile(
        result.ast!,
        outputFile,
        true,
        result.validationIssues,
        result.stats
      );

      const fileExists = await fs.access(outputFile).then(() => true).catch(() => false);
      expect(fileExists).toBe(true);

      const savedContent = await fs.readFile(outputFile, 'utf-8');
      const parsedContent = JSON.parse(savedContent);
      
      expect(parsedContent.ast).toBeDefined();
      expect(parsedContent.metadata).toBeDefined();
      expect(parsedContent.metadata.generated).toBeDefined();
      expect(parsedContent.metadata.statistics).toBeDefined();
    });
  });

  // Helper function to create temporary test files
  async function createTempFile(filename: string, content: string): Promise<string> {
    await fs.mkdir(tempDir, { recursive: true });
    const filePath = path.join(tempDir, filename);
    await fs.writeFile(filePath, content);
    return filePath;
  }
});
