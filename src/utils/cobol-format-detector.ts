import { CobolFormat } from '../types';
import { Logger } from './logger';

/**
 * Utility class for detecting and handling COBOL format conventions
 */
export class CobolFormatDetector {
  private logger: Logger;

  constructor(logger: Logger) {
    this.logger = logger;
  }

  /**
   * Detect COBOL format from source lines with detailed analysis
   */
  public detectFormat(lines: string[], fileName?: string): CobolFormat {
    this.logger.verbose(`Analyzing format for ${lines.length} lines${fileName ? ` in ${fileName}` : ''}`);

    const analysis = this.analyzeLines(lines);
    const isFixedFormat = this.determineFormatFromAnalysis(analysis);

    this.logger.verbose(
      `Format detection results for ${fileName || 'source'}:\n` +
      `  - Lines with sequence numbers: ${analysis.sequenceNumberLines}\n` +
      `  - Lines with column 7 indicators: ${analysis.indicatorLines}\n` +
      `  - Lines exceeding column 72: ${analysis.longLines}\n` +
      `  - Empty/comment lines: ${analysis.emptyOrCommentLines}\n` +
      `  - Total analyzed lines: ${analysis.totalLines}\n` +
      `  - Confidence: ${(analysis.confidence * 100).toFixed(1)}%\n` +
      `  - Detected format: ${isFixedFormat ? 'Fixed' : 'Free'}`
    );

    return {
      isFixedFormat,
      maxLineLength: isFixedFormat ? 72 : 255,
    };
  }

  /**
   * Analyze lines to gather format indicators
   */
  private analyzeLines(lines: string[]): FormatAnalysis {
    let sequenceNumberLines = 0;
    let indicatorLines = 0;
    let longLines = 0;
    let emptyOrCommentLines = 0;
    let totalLines = 0;
    let codeLines = 0;
    let properAreaALines = 0;

    for (const line of lines) {
      totalLines++;

      // Skip empty lines
      if (line.trim().length === 0) {
        emptyOrCommentLines++;
        continue;
      }

      // Skip lines that are too short to analyze
      if (line.length < 7) {
        continue;
      }

      codeLines++;

      // Check for sequence numbers in columns 1-6
      const sequenceArea = line.substring(0, 6).trim();
      if (this.looksLikeSequenceNumber(sequenceArea)) {
        sequenceNumberLines++;
      }

      // Check column 7 (indicator area)
      const indicator = line.charAt(6);
      if (this.isValidIndicator(indicator)) {
        indicatorLines++;
      }

      // Check for content that should be in Area A (columns 8-11)
      if (line.length > 7) {
        const areaA = line.substring(7, 11).trim();
        const areaB = line.length > 11 ? line.substring(11).trim() : '';
        
        if (this.looksLikeAreaAContent(areaA, areaB)) {
          properAreaALines++;
        }
      }

      // Check if line exceeds fixed format maximum (column 72)
      if (line.length > 72 && line.substring(72).trim().length > 0) {
        longLines++;
      }
    }

    // Calculate confidence based on how many indicators point to fixed format
    let confidence = 0;
    let indicators = 0;

    if (codeLines > 0) {
      // Sequence numbers
      const seqRatio = sequenceNumberLines / codeLines;
      if (seqRatio > 0.1) {
        confidence += seqRatio * 0.3;
        indicators++;
      }

      // Indicator area usage
      const indRatio = indicatorLines / totalLines;
      if (indRatio > 0.1) {
        confidence += indRatio * 0.4;
        indicators++;
      }

      // Proper Area A usage
      const areaARatio = properAreaALines / codeLines;
      if (areaARatio > 0.2) {
        confidence += areaARatio * 0.2;
        indicators++;
      }

      // Lines not exceeding column 72
      const shortLineRatio = 1 - (longLines / codeLines);
      if (shortLineRatio > 0.8) {
        confidence += shortLineRatio * 0.1;
        indicators++;
      }
    }

    if (indicators > 0) {
      confidence = confidence / Math.min(indicators, 1);
    }

    return {
      sequenceNumberLines,
      indicatorLines,
      longLines,
      emptyOrCommentLines,
      totalLines,
      codeLines,
      properAreaALines,
      confidence: Math.min(confidence, 1.0),
    };
  }

  /**
   * Determine format based on analysis results
   */
  private determineFormatFromAnalysis(analysis: FormatAnalysis): boolean {
    // If confidence is high enough, use it
    if (analysis.confidence > 0.6) {
      return true; // Fixed format
    }

    // Use heuristics for edge cases
    const hasStrongFixedIndicators = 
      analysis.sequenceNumberLines > 0 || 
      analysis.indicatorLines > (analysis.totalLines * 0.1);

    const hasStrongFreeIndicators = 
      analysis.longLines > (analysis.codeLines * 0.3);

    if (hasStrongFixedIndicators && !hasStrongFreeIndicators) {
      return true; // Fixed format
    }

    if (hasStrongFreeIndicators && !hasStrongFixedIndicators) {
      return false; // Free format
    }

    // Default to fixed format for ambiguous cases (most COBOL is fixed format)
    this.logger.verbose('Format detection ambiguous, defaulting to fixed format');
    return true;
  }

  /**
   * Check if text looks like a sequence number
   */
  private looksLikeSequenceNumber(text: string): boolean {
    if (text.length === 0) return false;
    
    // Sequence numbers are typically all digits, sometimes with leading zeros
    return /^\d{1,6}$/.test(text);
  }

  /**
   * Check if character is a valid COBOL indicator
   */
  private isValidIndicator(char: string): boolean {
    return char === '*' || char === '/' || char === '-' || char === ' ';
  }

  /**
   * Check if content looks like it belongs in Area A
   */
  private looksLikeAreaAContent(areaA: string, areaB: string): boolean {
    if (areaA.length === 0) return false;

    const upperAreaA = areaA.toUpperCase();
    const upperAreaB = areaB.toUpperCase();
    
    // Area A typically contains:
    // - Division names
    // - Section names
    // - Paragraph names
    // - Level numbers (01-49, 66, 77, 88)
    
    // Check for division headers
    if (upperAreaA.includes('DIVISION') || upperAreaB.includes('DIVISION')) {
      return true;
    }

    // Check for section headers
    if (upperAreaA.includes('SECTION') || upperAreaB.includes('SECTION')) {
      return true;
    }

    // Check for level numbers
    if (/^(0[1-9]|[1-4][0-9]|6[6]|7[7]|8[8])$/.test(areaA.trim())) {
      return true;
    }

    // Check for paragraph names (single identifier followed by period)
    const words = areaA.split(/\s+/);
    if (words.length === 1 && /^[A-Za-z][A-Za-z0-9-]*[A-Za-z0-9]?$/.test(words[0])) {
      // Check if the Area B starts with a period or is empty (suggesting paragraph header)
      if (areaB.startsWith('.') || areaB.trim().length === 0) {
        return true;
      }
    }

    return false;
  }

  /**
   * Validate format detection against known COBOL conventions
   */
  public validateFormat(format: CobolFormat, lines: string[]): FormatValidation {
    const warnings: string[] = [];
    const errors: string[] = [];
    
    if (format.isFixedFormat) {
      this.validateFixedFormat(lines, warnings, errors);
    } else {
      this.validateFreeFormat(lines, warnings, errors);
    }

    return {
      isValid: errors.length === 0,
      warnings,
      errors,
    };
  }

  /**
   * Validate fixed format conventions
   */
  private validateFixedFormat(lines: string[], warnings: string[], _errors: string[]): void {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const lineNum = i + 1;

      if (line.length === 0) continue;

      // Check for content in identification area (columns 73-80)
      if (line.length > 72) {
        const identArea = line.substring(72).trim();
        if (identArea.length > 0) {
          warnings.push(`Line ${lineNum}: Content in identification area (columns 73+): "${identArea}"`);
        }
      }

      // Check for invalid indicator
      if (line.length > 6) {
        const indicator = line.charAt(6);
        if (!this.isValidIndicator(indicator)) {
          warnings.push(`Line ${lineNum}: Unusual indicator in column 7: "${indicator}"`);
        }
      }

      // Check for very long lines
      if (line.length > 255) {
        warnings.push(`Line ${lineNum}: Extremely long line (${line.length} characters)`);
      }
    }
  }

  /**
   * Validate free format conventions
   */
  private validateFreeFormat(lines: string[], warnings: string[], _errors: string[]): void {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const lineNum = i + 1;

      if (line.length === 0) continue;

      // Free format is more flexible, but check for extremely long lines
      if (line.length > 1000) {
        warnings.push(`Line ${lineNum}: Very long line (${line.length} characters)`);
      }

      // Check for old-style sequence numbers
      if (line.length > 6) {
        const seqArea = line.substring(0, 6).trim();
        if (/^\d{6}$/.test(seqArea)) {
          warnings.push(`Line ${lineNum}: Looks like fixed format sequence numbers in free format`);
        }
      }
    }
  }
}

/**
 * Result of format analysis
 */
interface FormatAnalysis {
  sequenceNumberLines: number;
  indicatorLines: number;
  longLines: number;
  emptyOrCommentLines: number;
  totalLines: number;
  codeLines: number;
  properAreaALines: number;
  confidence: number;
}

/**
 * Result of format validation
 */
export interface FormatValidation {
  isValid: boolean;
  warnings: string[];
  errors: string[];
}
