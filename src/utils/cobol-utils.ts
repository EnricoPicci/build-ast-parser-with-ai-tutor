import { ProcessedLine } from '../types';

/**
 * COBOL-specific utility functions for string processing and format handling
 */
export class CobolUtils {
  
  /**
   * Normalize COBOL identifier names according to COBOL rules
   */
  static normalizeIdentifier(name: string): string {
    return name.toUpperCase().trim();
  }

  /**
   * Check if a string is a valid COBOL identifier
   */
  static isValidIdentifier(name: string): boolean {
    // COBOL identifiers can contain letters, digits, and hyphens
    // Must start with a letter, cannot end with hyphen
    const pattern = /^[A-Za-z][A-Za-z0-9-]*[A-Za-z0-9]$|^[A-Za-z]$/;
    return pattern.test(name);
  }

  /**
   * Extract words from a COBOL line, respecting COBOL word boundaries
   */
  static extractWords(line: string): string[] {
    // Split on whitespace and common COBOL delimiters
    const words = line
      .split(/[\s.,();]+/)
      .map(word => word.trim())
      .filter(word => word.length > 0);
    
    return words;
  }

  /**
   * Check if a line contains a COBOL division header
   */
  static isDivisionHeader(line: ProcessedLine): boolean {
    const content = line.content.toUpperCase();
    const divisions = [
      'IDENTIFICATION DIVISION',
      'ENVIRONMENT DIVISION', 
      'DATA DIVISION',
      'PROCEDURE DIVISION'
    ];
    
    return divisions.some(division => content.includes(division));
  }

  /**
   * Check if a line contains a COBOL section header
   */
  static isSectionHeader(line: ProcessedLine): boolean {
    const content = line.content.toUpperCase().trim();
    
    // Section headers end with "SECTION" and optionally a period
    if (content.endsWith('SECTION') || content.endsWith('SECTION.')) {
      // Make sure it's not part of a larger statement
      const words = this.extractWords(content);
      return words.length >= 2 && words[words.length - 1] === 'SECTION';
    }
    
    return false;
  }

  /**
   * Extract section name from a section header line
   */
  static extractSectionName(line: ProcessedLine): string | null {
    if (!this.isSectionHeader(line)) {
      return null;
    }
    
    const words = this.extractWords(line.content);
    if (words.length >= 2 && words[words.length - 1] === 'SECTION') {
      return words[words.length - 2];
    }
    
    return null;
  }

  /**
   * Check if a line contains a COBOL paragraph header
   */
  static isParagraphHeader(line: ProcessedLine): boolean {
    const content = line.content.trim();
    
    // Paragraph headers are typically single words ending with a period
    // and starting in Area A (but we'll be more flexible)
    if (content.endsWith('.')) {
      const withoutPeriod = content.slice(0, -1).trim();
      const words = this.extractWords(withoutPeriod);
      
      // Single word that looks like an identifier
      if (words.length === 1 && this.isValidIdentifier(words[0])) {
        // Make sure it's not a statement that ends with period
        return !this.isCobolStatement(withoutPeriod);
      }
    }
    
    return false;
  }

  /**
   * Extract paragraph name from a paragraph header line
   */
  static extractParagraphName(line: ProcessedLine): string | null {
    if (!this.isParagraphHeader(line)) {
      return null;
    }
    
    const content = line.content.trim();
    if (content.endsWith('.')) {
      const name = content.slice(0, -1).trim();
      return this.normalizeIdentifier(name);
    }
    
    return null;
  }

  /**
   * Check if a line contains a PERFORM statement
   */
  static isPerformStatement(line: ProcessedLine): boolean {
    const content = line.content.toUpperCase().trim();
    return content.startsWith('PERFORM ') || content === 'PERFORM';
  }

  /**
   * Check if text represents a COBOL statement (to distinguish from paragraph names)
   */
  static isCobolStatement(text: string): boolean {
    const content = text.toUpperCase().trim();
    const statements = [
      'ACCEPT', 'ADD', 'CALL', 'CANCEL', 'CLOSE', 'COMPUTE', 'CONTINUE',
      'DELETE', 'DISPLAY', 'DIVIDE', 'EVALUATE', 'EXIT', 'GO', 'GOBACK',
      'IF', 'INITIALIZE', 'INSPECT', 'INVOKE', 'MERGE', 'MOVE', 'MULTIPLY',
      'OPEN', 'PERFORM', 'READ', 'RELEASE', 'RETURN', 'REWRITE', 'SEARCH',
      'SET', 'SORT', 'START', 'STOP', 'STRING', 'SUBTRACT', 'UNSTRING',
      'WRITE'
    ];
    
    return statements.some(stmt => content.startsWith(stmt + ' ') || content === stmt);
  }

  /**
   * Clean and normalize COBOL content by removing extra whitespace
   */
  static cleanContent(content: string): string {
    return content
      .trim()
      .replace(/\s+/g, ' ') // Replace multiple spaces with single space
      .replace(/\s*\.\s*/g, '. ') // Normalize periods
      .replace(/\s*,\s*/g, ', '); // Normalize commas
  }

  /**
   * Check if a line is effectively empty (contains only whitespace or is a comment)
   */
  static isEffectivelyEmpty(line: ProcessedLine): boolean {
    return line.lineType === 'BLANK' || 
           line.lineType === 'COMMENT' || 
           line.content.trim().length === 0;
  }

  /**
   * Extract quoted strings from COBOL content
   */
  static extractQuotedStrings(content: string): string[] {
    const strings: string[] = [];
    const regex = /(['"])((?:(?!\1).)*)\1/g;
    let match;
    
    while ((match = regex.exec(content)) !== null) {
      strings.push(match[2]); // Content between quotes
    }
    
    return strings;
  }

  /**
   * Remove quoted strings from content to avoid parsing issues
   */
  static removeQuotedStrings(content: string, replacement: string = 'STRING'): string {
    return content.replace(/(['"])((?:(?!\1).)*)\1/g, replacement);
  }

  /**
   * Split a multi-statement line into individual statements
   */
  static splitStatements(content: string): string[] {
    // Simple approach: split on periods but be more careful about quotes
    if (!content.includes('.')) {
      return [content].filter(s => s.trim().length > 0);
    }

    const statements: string[] = [];
    let currentStatement = '';
    let inQuotes = false;
    let quoteChar = '';
    
    for (let i = 0; i < content.length; i++) {
      const char = content[i];
      
      if (!inQuotes && (char === '"' || char === "'")) {
        inQuotes = true;
        quoteChar = char;
        currentStatement += char;
      } else if (inQuotes && char === quoteChar) {
        inQuotes = false;
        quoteChar = '';
        currentStatement += char;
      } else if (!inQuotes && char === '.') {
        // End of statement
        currentStatement += char;
        const trimmed = currentStatement.trim();
        if (trimmed.length > 0) {
          // Remove the period from the end for the statement
          statements.push(trimmed.slice(0, -1).trim());
        }
        currentStatement = '';
      } else {
        currentStatement += char;
      }
    }
    
    // Add any remaining content
    const remaining = currentStatement.trim();
    if (remaining.length > 0) {
      statements.push(remaining);
    }
    
    return statements.filter(stmt => stmt.length > 0);
  }

  /**
   * Get the hierarchical level of a COBOL line based on indentation and content
   */
  static getHierarchicalLevel(line: ProcessedLine): number {
    // Division = 0, Section = 1, Paragraph = 2, Statement = 3
    if (this.isDivisionHeader(line)) return 0;
    if (this.isSectionHeader(line)) return 1;
    if (this.isParagraphHeader(line)) return 2;
    return 3; // Statement level
  }

  /**
   * Format error messages with COBOL context
   */
  static formatError(
    message: string, 
    line: ProcessedLine, 
    context?: string
  ): string {
    const location = `${line.sourceFile}:${line.originalLineNumber}`;
    const prefix = context ? `[${context}] ` : '';
    return `${prefix}${message} at ${location}: "${line.content.trim()}"`;
  }
}
