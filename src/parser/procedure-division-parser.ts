import { Logger } from '../utils/logger';
import {
  ProcessedLine,
  ProcedureDivisionNode,
  SectionNode,
  ParagraphNode,
} from '../types';
import { PerformAnalyzer } from './perform-analyzer';

/**
 * Parser for COBOL PROCEDURE DIVISION structure.
 * Identifies sections and paragraphs and builds the AST structure.
 */
export class ProcedureDivisionParser {
  private logger: Logger;
  private performAnalyzer: PerformAnalyzer;

  constructor(logger: Logger) {
    this.logger = logger;
    this.performAnalyzer = new PerformAnalyzer(logger);
  }

  /**
   * Parse the PROCEDURE DIVISION from processed lines
   */
  public parseProcedureDivision(lines: ProcessedLine[]): ProcedureDivisionNode | null {
    this.logger.verbose('Starting PROCEDURE DIVISION parsing');

    const procedureStart = this.findProcedureDivisionStart(lines);
    if (procedureStart === -1) {
      this.logger.verbose('No PROCEDURE DIVISION found');
      return null;
    }

    this.logger.verbose(`Found PROCEDURE DIVISION at line ${procedureStart + 1}`);

    // Extract lines from PROCEDURE DIVISION onwards
    const procedureLines = lines.slice(procedureStart);
    
    // Parse sections and paragraphs
    const { sections, paragraphs } = this.parseStructures(procedureLines);

    this.logger.verbose(`Found ${sections.length} sections and ${paragraphs.length} standalone paragraphs`);

    const procedureDivision: ProcedureDivisionNode = {
      type: 'PROCEDURE_DIVISION',
      children: [...sections, ...paragraphs],
      sections,
      paragraphs,
      sourceLines: procedureLines,
    };

    // Analyze PERFORM statements and build call relationships
    this.performAnalyzer.analyzePerformStatements(procedureDivision);

    return procedureDivision;
  }

  /**
   * Find the start of the PROCEDURE DIVISION
   */
  private findProcedureDivisionStart(lines: ProcessedLine[]): number {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (line.lineType === 'CODE' && this.isProcedureDivisionDeclaration(line.content)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Check if a line is a PROCEDURE DIVISION declaration
   */
  private isProcedureDivisionDeclaration(content: string): boolean {
    const trimmed = content.trim().toUpperCase();
    return trimmed.startsWith('PROCEDURE DIVISION');
  }

  /**
   * Parse sections and paragraphs from procedure lines
   */
  private parseStructures(lines: ProcessedLine[]): {
    sections: SectionNode[];
    paragraphs: ParagraphNode[];
  } {
    const sections: SectionNode[] = [];
    const standaloneParagraphs: ParagraphNode[] = [];
    
    let currentSection: SectionNode | null = null;
    let currentParagraph: ParagraphNode | null = null;
    let currentStructureLines: ProcessedLine[] = [];
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Skip non-code lines for structure detection, but include them in content
      if (line.lineType !== 'CODE') {
        currentStructureLines.push(line);
        continue;
      }

      const content = line.content.trim();
      
      // Check for section declaration
      if (this.isSectionDeclaration(content)) {
        this.logger.verbose(`DEBUG: Found section declaration: ${content}`);
        
        // Save previous paragraph if any
        if (currentParagraph) {
          this.finalizeParagraph(currentParagraph, currentStructureLines);
          if (currentSection) {
            currentSection.paragraphs.push(currentParagraph);
            currentSection.children.push(currentParagraph);
          } else {
            standaloneParagraphs.push(currentParagraph);
          }
          currentParagraph = null;
          currentStructureLines = [];
        }
        
        // Save previous section if any (section-level lines are in currentStructureLines)
        if (currentSection) {
          // Add any remaining section-level lines before closing the section
          if (currentStructureLines.length > 0) {
            currentSection.sourceLines.push(...currentStructureLines);
            this.logger.verbose(`DEBUG: Added ${currentStructureLines.length} final section-level lines to section ${currentSection.name}`);
          }
          sections.push(currentSection);
        }

        // Start new section
        const sectionName = this.extractSectionName(content);
        currentSection = {
          type: 'SECTION',
          name: sectionName,
          children: [],
          paragraphs: [],
          sourceLines: [line],
          performedBy: [],
          performs: [],
        };
        
        currentStructureLines = [];
        
        this.logger.verbose(`Found section: ${sectionName}`);
        continue;
      }
      
      // Check for paragraph declaration
      if (this.isParagraphDeclaration(content)) {
        this.logger.verbose(`DEBUG: Found paragraph declaration: ${content}`);
        
        // Save previous paragraph if any
        if (currentParagraph) {
          this.finalizeParagraph(currentParagraph, currentStructureLines);
          if (currentSection) {
            currentSection.paragraphs.push(currentParagraph);
            currentSection.children.push(currentParagraph);
          } else {
            standaloneParagraphs.push(currentParagraph);
          }
        } else if (currentSection && currentStructureLines.length > 0) {
          // If we're in a section with no current paragraph, these are section-level lines
          currentSection.sourceLines.push(...currentStructureLines);
          this.logger.verbose(`DEBUG: Added ${currentStructureLines.length} section-level lines to section ${currentSection.name}`);
        }

        // Start new paragraph
        const paragraphName = this.extractParagraphName(content);
        currentParagraph = {
          type: 'PARAGRAPH',
          name: paragraphName,
          children: [],
          sourceLines: [line],
          performedBy: [],
          performs: [],
        };
        
        currentStructureLines = [];
        
        this.logger.verbose(`Found paragraph: ${paragraphName}`);
        continue;
      }
      
      // Regular code line - add to current structure
      currentStructureLines.push(line);
    }
    
    // Save the last structure
    if (currentParagraph) {
      this.finalizeParagraph(currentParagraph, currentStructureLines);
      if (currentSection) {
        currentSection.paragraphs.push(currentParagraph);
        currentSection.children.push(currentParagraph);
      } else {
        standaloneParagraphs.push(currentParagraph);
      }
    } else if (currentSection && currentStructureLines.length > 0) {
      // If we're in a section with no current paragraph, add remaining lines to the section
      currentSection.sourceLines.push(...currentStructureLines);
      this.logger.verbose(`DEBUG: Added ${currentStructureLines.length} final section-level lines to section ${currentSection.name}`);
    }
    
    if (currentSection) {
      sections.push(currentSection);
    }

    return {
      sections,
      paragraphs: standaloneParagraphs,
    };
  }

  /**
   * Check if a line is a section declaration
   */
  private isSectionDeclaration(content: string): boolean {
    const trimmed = content.trim().toUpperCase();
    // Section declaration: "SECTION-NAME SECTION."
    return /^[A-Z][A-Z0-9-]*\s+SECTION\s*\.$/.test(trimmed);
  }

  /**
   * Check if a line is a paragraph declaration
   */
  private isParagraphDeclaration(content: string): boolean {
    const trimmed = content.trim().toUpperCase();
    
    // Skip PROCEDURE DIVISION declaration
    if (trimmed.startsWith('PROCEDURE DIVISION')) {
      return false;
    }
    
    // Skip section declarations
    if (this.isSectionDeclaration(trimmed)) {
      return false;
    }
    
    // Check if it matches paragraph pattern
    const match = trimmed.match(/^([A-Z][A-Z0-9-]*)\s*\.$/);
    if (!match) {
      return false;
    }
    
    const name = match[1];
    
    // Exclude COBOL reserved words and common statements that end with periods
    const reservedWords = [
      'END-IF', 'END-ELSE', 'END-PERFORM', 'END-READ', 'END-WRITE', 'END-REWRITE',
      'END-DELETE', 'END-START', 'END-ACCEPT', 'END-DISPLAY', 'END-CALL',
      'END-EVALUATE', 'END-SEARCH', 'END-STRING', 'END-UNSTRING', 'END-COMPUTE',
      'CONTINUE', 'EXIT', 'GOBACK', 'STOP', 'NEXT', 'ELSE'
    ];
    
    return !reservedWords.includes(name);
  }

  /**
   * Extract section name from declaration
   */
  private extractSectionName(content: string): string {
    const match = content.trim().toUpperCase().match(/^([A-Z][A-Z0-9-]*)\s+SECTION\s*\.$/);
    return match ? match[1] : 'UNKNOWN-SECTION';
  }

  /**
   * Extract paragraph name from declaration
   */
  private extractParagraphName(content: string): string {
    const match = content.trim().toUpperCase().match(/^([A-Z][A-Z0-9-]*)\s*\.$/);
    return match ? match[1] : 'UNKNOWN-PARAGRAPH';
  }



  /**
   * Finalize a paragraph by adding remaining lines
   */
  private finalizeParagraph(paragraph: ParagraphNode, lines: ProcessedLine[]): void {
    paragraph.sourceLines.push(...lines);
  }

  /**
   * Validate the parsed structure for correctness
   */
  public validateStructure(procedureDivision: ProcedureDivisionNode): string[] {
    const errors: string[] = [];

    // Check for empty structures
    if (procedureDivision.sections.length === 0 && procedureDivision.paragraphs.length === 0) {
      errors.push('PROCEDURE DIVISION contains no sections or paragraphs');
    }

    // Validate sections
    for (const section of procedureDivision.sections) {
      if (!section.name || section.name === 'UNKNOWN-SECTION') {
        errors.push(`Section found with invalid name: ${section.name}`);
      }
      
      if (section.sourceLines.length === 0) {
        errors.push(`Section '${section.name}' has no source lines`);
      }
    }

    // Validate paragraphs
    const allParagraphs = [
      ...procedureDivision.paragraphs,
      ...procedureDivision.sections.flatMap(s => s.paragraphs),
    ];

    for (const paragraph of allParagraphs) {
      if (!paragraph.name || paragraph.name === 'UNKNOWN-PARAGRAPH') {
        errors.push(`Paragraph found with invalid name: ${paragraph.name}`);
      }
      
      if (paragraph.sourceLines.length === 0) {
        errors.push(`Paragraph '${paragraph.name}' has no source lines`);
      }
    }

    // Check for duplicate names
    const sectionNames = procedureDivision.sections.map(s => s.name);
    const duplicateSections = sectionNames.filter((name, index) => sectionNames.indexOf(name) !== index);
    if (duplicateSections.length > 0) {
      errors.push(`Duplicate section names found: ${duplicateSections.join(', ')}`);
    }

    const paragraphNames = allParagraphs.map(p => p.name);
    const duplicateParagraphs = paragraphNames.filter((name, index) => paragraphNames.indexOf(name) !== index);
    if (duplicateParagraphs.length > 0) {
      errors.push(`Duplicate paragraph names found: ${duplicateParagraphs.join(', ')}`);
    }

    if (errors.length > 0) {
      this.logger.verbose(`Structure validation found ${errors.length} errors`);
    }

    return errors;
  }

  /**
   * Validate PERFORM targets in the parsed structure
   */
  public validatePerformTargets(procedureDivision: ProcedureDivisionNode): string[] {
    return this.performAnalyzer.validatePerformTargets(procedureDivision);
  }

  /**
   * Generate a call graph report for the PROCEDURE DIVISION
   */
  public generateCallGraphReport(procedureDivision: ProcedureDivisionNode): string {
    return this.performAnalyzer.generateCallGraphReport(procedureDivision);
  }

  /**
   * Get the symbol table from the PERFORM analyzer (for testing/debugging)
   */
  public getSymbolTable(): Map<string, SectionNode | ParagraphNode> {
    return this.performAnalyzer.getSymbolTable();
  }
}
