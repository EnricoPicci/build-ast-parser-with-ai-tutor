import { Logger } from '../utils/logger';
import {
  ProcessedLine,
  PerformReference,
  ProcedureDivisionNode,
  SectionNode,
  ParagraphNode,
} from '../types';

/**
 * Analyzer for PERFORM statements in COBOL code.
 * Identifies PERFORM statements and builds call relationships between procedures.
 */
export class PerformAnalyzer {
  private logger: Logger;
  private symbolTable: Map<string, SectionNode | ParagraphNode> = new Map();

  constructor(logger: Logger) {
    this.logger = logger;
  }

  /**
   * Analyze PERFORM statements in the given PROCEDURE DIVISION
   */
  public analyzePerformStatements(procedureDivision: ProcedureDivisionNode): void {
    this.logger.verbose('Starting PERFORM statement analysis');

    // Build symbol table for procedure name lookup
    this.buildSymbolTable(procedureDivision);

    // Analyze PERFORM statements in all procedures
    this.analyzeAllProcedures(procedureDivision);

    this.logger.verbose('PERFORM statement analysis completed');
  }

  /**
   * Build a symbol table for fast procedure name lookup
   */
  private buildSymbolTable(procedureDivision: ProcedureDivisionNode): void {
    this.symbolTable.clear();

    // Add standalone paragraphs
    for (const paragraph of procedureDivision.paragraphs) {
      this.symbolTable.set(paragraph.name.toUpperCase(), paragraph);
    }

    // Add sections and their paragraphs
    for (const section of procedureDivision.sections) {
      this.symbolTable.set(section.name.toUpperCase(), section);
      
      for (const paragraph of section.paragraphs) {
        this.symbolTable.set(paragraph.name.toUpperCase(), paragraph);
      }
    }

    this.logger.verbose(`Built symbol table with ${this.symbolTable.size} procedures`);
  }

  /**
   * Analyze PERFORM statements in all procedures
   */
  private analyzeAllProcedures(procedureDivision: ProcedureDivisionNode): void {
    // Analyze standalone paragraphs
    for (const paragraph of procedureDivision.paragraphs) {
      this.analyzeProcedurePerforms(paragraph);
    }

    // Analyze sections and their paragraphs
    for (const section of procedureDivision.sections) {
      // Analyze section-level PERFORM statements
      this.analyzeProcedurePerforms(section);
      
      // Analyze paragraphs within sections
      for (const paragraph of section.paragraphs) {
        this.analyzeProcedurePerforms(paragraph);
      }
    }
  }

  /**
   * Analyze PERFORM statements in a single procedure (section or paragraph)
   */
  private analyzeProcedurePerforms(procedure: SectionNode | ParagraphNode): void {
    const performReferences: PerformReference[] = [];

    for (const line of procedure.sourceLines) {
      if (line.lineType === 'CODE') {
        const performs = this.extractPerformStatements(line);
        performReferences.push(...performs);
      }
    }

    // Add perform references to the procedure
    procedure.performs = performReferences;

    // Update target procedures with "performed by" references
    for (const performRef of performReferences) {
      this.addPerformedByReference(performRef, procedure);
    }

    if (performReferences.length > 0) {
      this.logger.verbose(`Found ${performReferences.length} PERFORM statements in ${procedure.name}`);
    }
  }

  /**
   * Extract PERFORM statements from a source line
   */
  private extractPerformStatements(line: ProcessedLine): PerformReference[] {
    const content = line.content.trim().toUpperCase();
    const performs: PerformReference[] = [];

    // Skip if line doesn't contain PERFORM
    if (!content.includes('PERFORM')) {
      return performs;
    }

    // Handle different PERFORM patterns
    const simplePerformMatch = this.matchSimplePerform(content);
    if (simplePerformMatch) {
      performs.push({
        targetName: simplePerformMatch.target,
        performType: 'SIMPLE',
        sourceLine: line,
      });
    }

    const throughPerformMatch = this.matchThroughPerform(content);
    if (throughPerformMatch) {
      performs.push({
        targetName: throughPerformMatch.startTarget,
        performType: 'THROUGH',
        sourceLine: line,
        throughTarget: throughPerformMatch.endTarget,
      });
    }

    const timesPerformMatch = this.matchTimesPerform(content);
    if (timesPerformMatch) {
      performs.push({
        targetName: timesPerformMatch.target,
        performType: 'TIMES',
        sourceLine: line,
      });
    }

    const untilPerformMatch = this.matchUntilPerform(content);
    if (untilPerformMatch) {
      performs.push({
        targetName: untilPerformMatch.target,
        performType: 'UNTIL',
        sourceLine: line,
      });
    }

    const varyingPerformMatch = this.matchVaryingPerform(content);
    if (varyingPerformMatch) {
      performs.push({
        targetName: varyingPerformMatch.target,
        performType: 'VARYING',
        sourceLine: line,
      });
    }

    return performs;
  }

  /**
   * Match simple PERFORM statement: PERFORM PARAGRAPH-NAME
   */
  private matchSimplePerform(content: string): { target: string } | null {
    // Pattern: PERFORM followed by procedure name (and possibly ending with period)
    const match = content.match(/^\s*PERFORM\s+([A-Z][A-Z0-9-]*)\s*\.?\s*$/);
    if (match) {
      return { target: match[1] };
    }
    return null;
  }

  /**
   * Match PERFORM THROUGH statement: PERFORM PARA-1 THROUGH PARA-5
   */
  private matchThroughPerform(content: string): { startTarget: string; endTarget: string } | null {
    // Pattern: PERFORM start-name THROUGH end-name
    const match = content.match(/^\s*PERFORM\s+([A-Z][A-Z0-9-]*)\s+THROUGH\s+([A-Z][A-Z0-9-]*)\s*\.?\s*$/);
    if (match) {
      return { startTarget: match[1], endTarget: match[2] };
    }
    return null;
  }

  /**
   * Match PERFORM TIMES statement: PERFORM PARAGRAPH-NAME 5 TIMES
   */
  private matchTimesPerform(content: string): { target: string } | null {
    // Pattern: PERFORM procedure-name number TIMES
    const match = content.match(/^\s*PERFORM\s+([A-Z][A-Z0-9-]*)\s+\d+\s+TIMES\s*\.?\s*$/);
    if (match) {
      return { target: match[1] };
    }
    
    // Pattern: PERFORM procedure-name variable TIMES
    const variableMatch = content.match(/^\s*PERFORM\s+([A-Z][A-Z0-9-]*)\s+[A-Z][A-Z0-9-]*\s+TIMES\s*\.?\s*$/);
    if (variableMatch) {
      return { target: variableMatch[1] };
    }
    
    return null;
  }

  /**
   * Match PERFORM UNTIL statement: PERFORM PARAGRAPH-NAME UNTIL condition
   */
  private matchUntilPerform(content: string): { target: string } | null {
    // Pattern: PERFORM procedure-name UNTIL condition
    const match = content.match(/^\s*PERFORM\s+([A-Z][A-Z0-9-]*)\s+UNTIL\s+.+$/);
    if (match) {
      return { target: match[1] };
    }
    return null;
  }

  /**
   * Match PERFORM VARYING statement: PERFORM PARAGRAPH-NAME VARYING variable FROM 1 BY 1 UNTIL condition
   */
  private matchVaryingPerform(content: string): { target: string } | null {
    // Pattern: PERFORM procedure-name VARYING variable FROM value BY value UNTIL condition
    const match = content.match(/^\s*PERFORM\s+([A-Z][A-Z0-9-]*)\s+VARYING\s+.+$/);
    if (match) {
      return { target: match[1] };
    }
    return null;
  }

  /**
   * Add a "performed by" reference to the target procedure
   */
  private addPerformedByReference(performRef: PerformReference, caller: SectionNode | ParagraphNode): void {
    const targetName = performRef.targetName.toUpperCase();
    const target = this.symbolTable.get(targetName);

    if (target) {
      const performedByRef: PerformReference = {
        targetName: caller.name,
        performType: performRef.performType,
        sourceLine: performRef.sourceLine,
      };
      if (performRef.throughTarget) {
        performedByRef.throughTarget = performRef.throughTarget;
      }
      target.performedBy.push(performedByRef);
    } else {
      this.logger.warn(`PERFORM target '${performRef.targetName}' not found in symbol table (line ${performRef.sourceLine.originalLineNumber})`);
    }

    // For THROUGH statements, also add reference to the end target
    if (performRef.throughTarget) {
      const endTargetName = performRef.throughTarget.toUpperCase();
      const endTarget = this.symbolTable.get(endTargetName);
      
      if (endTarget) {
        const endPerformedByRef: PerformReference = {
          targetName: caller.name,
          performType: performRef.performType,
          sourceLine: performRef.sourceLine,
        };
        if (performRef.throughTarget) {
          endPerformedByRef.throughTarget = performRef.throughTarget;
        }
        endTarget.performedBy.push(endPerformedByRef);
      } else {
        this.logger.warn(`PERFORM THROUGH end target '${performRef.throughTarget}' not found in symbol table (line ${performRef.sourceLine.originalLineNumber})`);
      }
    }
  }

  /**
   * Validate all PERFORM targets exist
   */
  public validatePerformTargets(procedureDivision: ProcedureDivisionNode): string[] {
    const errors: string[] = [];
    const allProcedures = [
      ...procedureDivision.paragraphs,
      ...procedureDivision.sections,
      ...procedureDivision.sections.flatMap(s => s.paragraphs),
    ];

    for (const procedure of allProcedures) {
      for (const performRef of procedure.performs) {
        const targetName = performRef.targetName.toUpperCase();
        if (!this.symbolTable.has(targetName)) {
          errors.push(`PERFORM target '${performRef.targetName}' not found (line ${performRef.sourceLine.originalLineNumber})`);
        }

        // Validate THROUGH target as well
        if (performRef.throughTarget) {
          const throughTargetName = performRef.throughTarget.toUpperCase();
          if (!this.symbolTable.has(throughTargetName)) {
            errors.push(`PERFORM THROUGH target '${performRef.throughTarget}' not found (line ${performRef.sourceLine.originalLineNumber})`);
          }
        }
      }
    }

    return errors;
  }

  /**
   * Get the symbol table for debugging/testing purposes
   */
  public getSymbolTable(): Map<string, SectionNode | ParagraphNode> {
    return this.symbolTable;
  }

  /**
   * Generate call graph report
   */
  public generateCallGraphReport(procedureDivision: ProcedureDivisionNode): string {
    const allProcedures = [
      ...procedureDivision.paragraphs,
      ...procedureDivision.sections,
      ...procedureDivision.sections.flatMap(s => s.paragraphs),
    ];

    let report = 'PROCEDURE CALL GRAPH\n';
    report += '====================\n\n';

    for (const procedure of allProcedures) {
      report += `${procedure.type}: ${procedure.name}\n`;
      
      if (procedure.performs.length > 0) {
        report += '  Calls:\n';
        for (const performRef of procedure.performs) {
          report += `    - ${performRef.targetName} (${performRef.performType}`;
          if (performRef.throughTarget) {
            report += ` THROUGH ${performRef.throughTarget}`;
          }
          report += `, line ${performRef.sourceLine.originalLineNumber})\n`;
        }
      }

      if (procedure.performedBy.length > 0) {
        report += '  Called by:\n';
        for (const callerRef of procedure.performedBy) {
          report += `    - ${callerRef.targetName} (${callerRef.performType}`;
          if (callerRef.throughTarget) {
            report += ` THROUGH ${callerRef.throughTarget}`;
          }
          report += `, line ${callerRef.sourceLine.originalLineNumber})\n`;
        }
      }

      if (procedure.performs.length === 0 && procedure.performedBy.length === 0) {
        report += '  No call relationships\n';
      }

      report += '\n';
    }

    return report;
  }
}
