import { Logger } from '../utils/logger';
import { FileHandler } from '../utils/file-handler';
import { CobolFormatDetector } from '../utils/cobol-format-detector';
import { LineProcessor } from './line-processor';
import { ProcedureDivisionParser } from './procedure-division-parser';
import { PerformAnalyzer } from './perform-analyzer';
import {
  ProcessedLine,
  ProgramNode,
  ProcedureDivisionNode,
  SectionNode,
  ParagraphNode,
  ASTNode,
} from '../types';

/**
 * Result of AST building process
 */
export interface ASTBuildResult {
  /** Indicates if AST building was successful */
  success: boolean;
  /** The built AST (if successful) */
  ast?: ProgramNode;
  /** Error message if building failed */
  error?: string;
  /** Validation errors and warnings */
  validationIssues?: ValidationIssue[];
  /** Statistics about the parsing process */
  stats?: ASTBuildStats;
}

/**
 * Validation issue found during AST building
 */
export interface ValidationIssue {
  /** Type of issue */
  type: 'ERROR' | 'WARNING';
  /** Description of the issue */
  message: string;
  /** Line number where issue occurs (if applicable) */
  lineNumber?: number;
  /** Source file where issue occurs */
  sourceFile?: string;
  /** Related AST node (if applicable) */
  node?: ASTNode;
}

/**
 * Statistics about the AST building process
 */
export interface ASTBuildStats {
  /** Total number of lines processed */
  totalLines: number;
  /** Number of procedure division lines */
  procedureLines: number;
  /** Number of sections found */
  sectionCount: number;
  /** Number of paragraphs found */
  paragraphCount: number;
  /** Number of PERFORM statements found */
  performCount: number;
  /** Number of validation errors */
  errorCount: number;
  /** Number of validation warnings */
  warningCount: number;
  /** Processing time in milliseconds */
  processingTime: number;
}

/**
 * Main AST builder that orchestrates the complete parsing process
 */
export class ASTBuilder {
  private logger: Logger;
  private fileHandler: FileHandler;
  private formatDetector: CobolFormatDetector;
  private lineProcessor: LineProcessor;
  private procedureDivisionParser: ProcedureDivisionParser;
  private performAnalyzer: PerformAnalyzer;

  constructor(logger: Logger) {
    this.logger = logger;
    this.fileHandler = new FileHandler(logger);
    this.formatDetector = new CobolFormatDetector(logger);
    this.lineProcessor = new LineProcessor(logger);
    this.procedureDivisionParser = new ProcedureDivisionParser(logger);
    this.performAnalyzer = new PerformAnalyzer(logger);
  }

  /**
   * Build complete AST from COBOL source file
   */
  public async buildAST(filePath: string): Promise<ASTBuildResult> {
    const startTime = Date.now();
    this.logger.verbose(`Building AST for file: ${filePath}`);

    try {
      // Step 1: Read source file
      const content = await this.fileHandler.readCobolFile(filePath);
      const rawLines = content.split('\n');

      // Step 2: Detect format and process lines
      const format = this.formatDetector.detectFormat(rawLines);
      this.lineProcessor = new LineProcessor(this.logger, { format });
      const lineProcessingResult = this.lineProcessor.processLines(rawLines, filePath);

      this.logger.verbose(`Processed ${lineProcessingResult.processedLineCount} lines`);

      // Step 3: Extract program name
      const programName = this.extractProgramName(lineProcessingResult.lines, filePath);

      // Step 4: Parse PROCEDURE DIVISION structure
      const procedureDivision = this.procedureDivisionParser.parseProcedureDivision(
        lineProcessingResult.lines
      );

      // Step 5: Analyze PERFORM statements (if PROCEDURE DIVISION exists)
      if (procedureDivision) {
        this.performAnalyzer.analyzePerformStatements(procedureDivision);
      }

      // Step 6: Build complete AST
      const ast = this.buildProgramNode(
        programName,
        procedureDivision,
        lineProcessingResult.lines
      );

      // Step 7: Validate AST
      const validationIssues = this.validateAST(ast);

      // Step 8: Calculate statistics
      const stats = this.calculateStats(
        lineProcessingResult,
        procedureDivision,
        validationIssues,
        Date.now() - startTime
      );

      this.logger.verbose(`AST building completed in ${stats.processingTime}ms`);

      return {
        success: true,
        ast,
        validationIssues,
        stats,
      };
    } catch (error) {
      this.logger.error(`AST building failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        stats: {
          totalLines: 0,
          procedureLines: 0,
          sectionCount: 0,
          paragraphCount: 0,
          performCount: 0,
          errorCount: 1,
          warningCount: 0,
          processingTime: Date.now() - startTime,
        },
      };
    }
  }

  /**
   * Extract program name from source lines
   */
  private extractProgramName(lines: ProcessedLine[], filePath: string): string {
    // Look for PROGRAM-ID statement
    for (const line of lines) {
      const content = line.content.toUpperCase().trim();
      if (content.startsWith('PROGRAM-ID')) {
        const match = content.match(/PROGRAM-ID\s*\.\s*([A-Z0-9-]+)/);
        if (match) {
          this.logger.verbose(`Found program name: ${match[1]}`);
          return match[1];
        }
      }
    }

    // Fallback: use filename without extension
    const filename = filePath.split(/[/\\]/).pop() || 'UNKNOWN';
    const programName = filename.replace(/\.[^.]*$/, '').toUpperCase();
    this.logger.verbose(`Using filename as program name: ${programName}`);
    return programName;
  }

  /**
   * Build the root program node
   */
  private buildProgramNode(
    programName: string,
    procedureDivision: ProcedureDivisionNode | null,
    sourceLines: ProcessedLine[]
  ): ProgramNode {
    const children: ASTNode[] = [];
    if (procedureDivision) {
      children.push(procedureDivision);
    }

    const programNode: ProgramNode = {
      type: 'PROGRAM',
      name: programName,
      sourceLines,
      children,
    };

    if (procedureDivision) {
      programNode.procedureDivision = procedureDivision;
    }

    return programNode;
  }

  /**
   * Validate the built AST for structural correctness and semantic issues
   */
  private validateAST(ast: ProgramNode): ValidationIssue[] {
    const issues: ValidationIssue[] = [];

    // Validate program structure
    if (!ast.name || ast.name.trim() === '') {
      issues.push({
        type: 'WARNING',
        message: 'Program name is empty or missing',
      });
    }

    if (!ast.procedureDivision) {
      issues.push({
        type: 'WARNING',
        message: 'No PROCEDURE DIVISION found in program',
      });
      return issues; // No further validation needed
    }

    // Validate PROCEDURE DIVISION structure
    const procedureDivision = ast.procedureDivision;
    
    // Check for duplicate section/paragraph names
    const nameTracker = new Set<string>();
    
    // Check sections
    for (const section of procedureDivision.sections) {
      const upperName = section.name.toUpperCase();
      if (nameTracker.has(upperName)) {
        issues.push({
          type: 'ERROR',
          message: `Duplicate section name: ${section.name}`,
          node: section,
        });
      } else {
        nameTracker.add(upperName);
      }

      // Check paragraphs within section
      for (const paragraph of section.paragraphs) {
        const upperParagraphName = paragraph.name.toUpperCase();
        if (nameTracker.has(upperParagraphName)) {
          issues.push({
            type: 'ERROR',
            message: `Duplicate paragraph name: ${paragraph.name}`,
            node: paragraph,
          });
        } else {
          nameTracker.add(upperParagraphName);
        }
      }
    }

    // Check standalone paragraphs
    for (const paragraph of procedureDivision.paragraphs) {
      const upperName = paragraph.name.toUpperCase();
      if (nameTracker.has(upperName)) {
        issues.push({
          type: 'ERROR',
          message: `Duplicate paragraph name: ${paragraph.name}`,
          node: paragraph,
        });
      } else {
        nameTracker.add(upperName);
      }
    }

    // Validate PERFORM references
    this.validatePerformReferences(procedureDivision, nameTracker, issues);

    return issues;
  }

  /**
   * Validate PERFORM statement references
   */
  private validatePerformReferences(
    procedureDivision: ProcedureDivisionNode,
    availableTargets: Set<string>,
    issues: ValidationIssue[]
  ): void {
    const validateProcedurePerforms = (procedure: SectionNode | ParagraphNode) => {
      for (const performRef of procedure.performs) {
        const targetName = performRef.targetName.toUpperCase();
        
        if (!availableTargets.has(targetName)) {
          issues.push({
            type: 'ERROR',
            message: `PERFORM target not found: ${performRef.targetName}`,
            lineNumber: performRef.sourceLine.originalLineNumber,
            sourceFile: performRef.sourceLine.sourceFile,
            node: procedure,
          });
        }

        // Validate THROUGH target if present
        if (performRef.throughTarget) {
          const throughTargetName = performRef.throughTarget.toUpperCase();
          if (!availableTargets.has(throughTargetName)) {
            issues.push({
              type: 'ERROR',
              message: `PERFORM THROUGH target not found: ${performRef.throughTarget}`,
              lineNumber: performRef.sourceLine.originalLineNumber,
              sourceFile: performRef.sourceLine.sourceFile,
              node: procedure,
            });
          }
        }
      }
    };

    // Validate PERFORM references in sections
    for (const section of procedureDivision.sections) {
      validateProcedurePerforms(section);
      
      // Validate PERFORM references in paragraphs within sections
      for (const paragraph of section.paragraphs) {
        validateProcedurePerforms(paragraph);
      }
    }

    // Validate PERFORM references in standalone paragraphs
    for (const paragraph of procedureDivision.paragraphs) {
      validateProcedurePerforms(paragraph);
    }
  }

  /**
   * Calculate build statistics
   */
  private calculateStats(
    lineProcessingResult: any,
    procedureDivision: ProcedureDivisionNode | null,
    validationIssues: ValidationIssue[],
    processingTime: number
  ): ASTBuildStats {
    const errorCount = validationIssues.filter(issue => issue.type === 'ERROR').length;
    const warningCount = validationIssues.filter(issue => issue.type === 'WARNING').length;

    let sectionCount = 0;
    let paragraphCount = 0;
    let performCount = 0;

    if (procedureDivision) {
      sectionCount = procedureDivision.sections.length;
      paragraphCount = procedureDivision.paragraphs.length;

      // Count paragraphs within sections
      for (const section of procedureDivision.sections) {
        paragraphCount += section.paragraphs.length;
      }

      // Count PERFORM statements
      const countPerforms = (procedure: SectionNode | ParagraphNode) => {
        performCount += procedure.performs.length;
      };

      procedureDivision.sections.forEach(section => {
        countPerforms(section);
        section.paragraphs.forEach(countPerforms);
      });
      procedureDivision.paragraphs.forEach(countPerforms);
    }

    return {
      totalLines: lineProcessingResult.originalLineCount,
      procedureLines: procedureDivision ? procedureDivision.sourceLines.length : 0,
      sectionCount,
      paragraphCount,
      performCount,
      errorCount,
      warningCount,
      processingTime,
    };
  }

  /**
   * Serialize AST to JSON format
   */
  public serializeToJSON(ast: ProgramNode, prettyPrint: boolean = true): string {
    const indent = prettyPrint ? 2 : 0;
    return JSON.stringify(ast, this.createJSONReplacer(), indent);
  }

  /**
   * Create custom JSON replacer to handle AST serialization
   */
  private createJSONReplacer(): (key: string, value: any) => any {
    return (_key: string, value: any) => {
      // Ensure consistent property ordering for AST nodes
      if (value && typeof value === 'object' && value.type) {
        const ordered: any = { type: value.type };
        
        // Add name if present
        if (value.name !== undefined) {
          ordered.name = value.name;
        }

        // Add specific properties based on node type
        switch (value.type) {
          case 'PROGRAM':
            if (value.procedureDivision) ordered.procedureDivision = value.procedureDivision;
            if (value.sourceLines) ordered.sourceLines = value.sourceLines;
            if (value.children) ordered.children = value.children;
            break;

          case 'PROCEDURE_DIVISION':
            if (value.sections) ordered.sections = value.sections;
            if (value.paragraphs) ordered.paragraphs = value.paragraphs;
            if (value.sourceLines) ordered.sourceLines = value.sourceLines;
            if (value.children) ordered.children = value.children;
            break;

          case 'SECTION':
          case 'PARAGRAPH':
            if (value.paragraphs) ordered.paragraphs = value.paragraphs;
            if (value.sourceLines) ordered.sourceLines = value.sourceLines;
            if (value.performedBy) ordered.performedBy = value.performedBy;
            if (value.performs) ordered.performs = value.performs;
            if (value.children) ordered.children = value.children;
            break;

          default:
            // For other objects, maintain original structure
            return value;
        }

        return ordered;
      }

      return value;
    };
  }

  /**
   * Generate output filename for AST JSON
   */
  public generateOutputFilename(inputFilePath: string): string {
    const pathParts = inputFilePath.split(/[/\\]/);
    const filename = pathParts[pathParts.length - 1];
    const nameWithoutExt = filename.replace(/\.[^.]*$/, '');
    const directory = pathParts.slice(0, -1).join('/');
    
    const outputFilename = `${nameWithoutExt}.ast.json`;
    return directory ? `${directory}/${outputFilename}` : outputFilename;
  }

  /**
   * Save AST to file
   */
  public async saveASTToFile(
    ast: ProgramNode,
    outputPath: string,
    includeValidationInfo: boolean = true,
    validationIssues?: ValidationIssue[],
    stats?: ASTBuildStats
  ): Promise<void> {
    const astJson = this.serializeToJSON(ast, true);
    
    if (includeValidationInfo && (validationIssues || stats)) {
      // Create enhanced output with metadata
      const output = {
        ast: JSON.parse(astJson),
        metadata: {
          generated: new Date().toISOString(),
          generator: 'COBOL AST Parser v1.0',
          ...(stats && { statistics: stats }),
          ...(validationIssues && { validationIssues }),
        },
      };
      
      await this.fileHandler.writeFile(outputPath, JSON.stringify(output, null, 2));
    } else {
      await this.fileHandler.writeFile(outputPath, astJson);
    }

    this.logger.verbose(`AST saved to: ${outputPath}`);
  }
}
