import { Logger } from '../utils/logger';
import {
  ProcessedLine,
  LineProcessorConfig,
  LineProcessingResult,
  CobolFormat,
} from '../types';

/**
 * Handles COBOL-specific line processing including format detection,
 * comment handling, and continuation line processing
 */
export class LineProcessor {
  private logger: Logger;
  private config: LineProcessorConfig;

  constructor(logger: Logger, config?: Partial<LineProcessorConfig>) {
    this.logger = logger;
    
    // Default configuration for COBOL line processing
    this.config = {
      format: {
        isFixedFormat: true,
        maxLineLength: 72,
      },
      preserveComments: false,
      preserveBlankLines: false,
      normalizeCase: true,
      ...config,
    };
  }

  /**
   * Process an array of raw COBOL lines
   */
  public processLines(
    rawLines: string[],
    sourceFile: string = 'unknown'
  ): LineProcessingResult {
    this.logger.verbose(`Processing ${rawLines.length} lines from ${sourceFile}`);

    const detectedFormat = this.detectFormat(rawLines);
    this.logger.verbose(`Detected format: ${detectedFormat.isFixedFormat ? 'Fixed' : 'Free'}`);

    const processedLines: ProcessedLine[] = [];
    let commentLineCount = 0;
    let continuationLineCount = 0;
    let pendingContinuation: ProcessedLine | null = null;

    for (let i = 0; i < rawLines.length; i++) {
      const rawLine = rawLines[i];
      const lineNumber = i + 1;

      // Skip null or undefined lines
      if (rawLine === null || rawLine === undefined) {
        this.logger.warn(`Null or undefined line at position ${lineNumber} in ${sourceFile}`);
        continue;
      }

      try {
        const processed = this.processLine(
          rawLine,
          lineNumber,
          sourceFile,
          detectedFormat
        );

        if (processed.lineType === 'COMMENT') {
          commentLineCount++;
          if (this.config.preserveComments) {
            processedLines.push(processed);
          }
          continue;
        }

        if (processed.lineType === 'BLANK') {
          if (this.config.preserveBlankLines) {
            processedLines.push(processed);
          }
          continue;
        }

        if (processed.lineType === 'CONTINUATION') {
          continuationLineCount++;
          if (pendingContinuation) {
            // Combine with previous continuation
            pendingContinuation = this.combineContinuationLines(
              pendingContinuation,
              processed
            );
          } else {
            this.logger.warn(
              `Continuation line without preceding line at ${sourceFile}:${lineNumber}`
            );
            // Treat as regular line
            processedLines.push(processed);
          }
          continue;
        }

        // If we have a pending continuation, finalize it
        if (pendingContinuation) {
          processedLines.push(pendingContinuation);
          pendingContinuation = null;
        }

        // Check if this line ends with a continuation indicator
        if (this.isLineToBecontinued(processed, rawLines[i + 1])) {
          pendingContinuation = processed;
        } else {
          processedLines.push(processed);
        }
      } catch (error) {
        this.logger.error(
          `Error processing line ${lineNumber} in ${sourceFile}: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
        
        // Create a fallback processed line
        const fallbackLine: ProcessedLine = {
          content: rawLine.trim(),
          originalLineNumber: lineNumber,
          sourceFile,
          isFromCopy: false,
          lineType: 'CODE',
          rawContent: rawLine,
        };
        processedLines.push(fallbackLine);
      }
    }

    // Handle any remaining continuation
    if (pendingContinuation) {
      processedLines.push(pendingContinuation);
    }

    const result: LineProcessingResult = {
      lines: processedLines,
      originalLineCount: rawLines.length,
      processedLineCount: processedLines.length,
      commentLineCount,
      continuationLineCount,
      detectedFormat,
    };

    this.logger.verbose(
      `Processed ${result.processedLineCount} lines from ${result.originalLineCount} original lines`
    );

    return result;
  }

  /**
   * Process a single line according to COBOL format rules
   */
  private processLine(
    rawLine: string,
    lineNumber: number,
    sourceFile: string,
    format: CobolFormat
  ): ProcessedLine {
    const base: ProcessedLine = {
      content: '',
      originalLineNumber: lineNumber,
      sourceFile,
      isFromCopy: false,
      lineType: 'CODE',
      rawContent: rawLine,
    };

    // Handle empty or very short lines
    if (rawLine.trim().length === 0) {
      return { ...base, lineType: 'BLANK', content: '' };
    }

    if (format.isFixedFormat) {
      return this.processFixedFormatLine(rawLine, base);
    } else {
      return this.processFreeFormatLine(rawLine, base);
    }
  }

  /**
   * Process a line in fixed COBOL format
   */
  private processFixedFormatLine(rawLine: string, base: ProcessedLine): ProcessedLine {
    // For very short lines, treat them as free format 
    if (rawLine.length < 7) {
      return this.processFreeFormatLine(rawLine, base);
    }
    
    // Pad line to at least 7 characters to check indicator area
    const paddedLine = rawLine.padEnd(7, ' ');
    
    // Extract areas according to COBOL fixed format
    // const sequenceArea = paddedLine.substring(0, 6); // Columns 1-6 (ignored)
    const indicatorArea = paddedLine.length > 6 ? paddedLine.charAt(6) : ' '; // Column 7
    const areaA = paddedLine.length > 7 ? paddedLine.substring(7, 11) : ''; // Columns 8-11
    const areaB = paddedLine.length > 11 ? paddedLine.substring(11, 72) : ''; // Columns 12-72
    
    base.indicatorArea = indicatorArea;
    base.areaA = areaA.trimEnd();
    base.areaB = areaB.trimEnd();

    // Check indicator area for special meanings
    switch (indicatorArea) {
      case '*':
      case '/':
        // Comment line
        base.lineType = 'COMMENT';
        base.content = paddedLine.substring(7).trimEnd();
        break;
      
      case '-':
        // Continuation line
        base.lineType = 'CONTINUATION';
        // Combine areas properly
        let contContent = areaA.trimEnd();
        const contAreaB = areaB.trimStart();
        
        if (contContent.length > 0 && contAreaB.length > 0) {
          if (/[\s\-.]$/.test(areaA)) {
            contContent += ' ' + contAreaB;
          } else {
            contContent += contAreaB;
          }
        } else {
          contContent += contAreaB;
        }
        base.content = contContent.trim();
        break;
      
      case ' ':
      case undefined:
        // Regular code line
        base.lineType = 'CODE';
        // Combine areas properly
        let content = areaA.trimEnd();
        const areaBTrimmed = areaB.trimStart();
        
        if (content.length > 0 && areaBTrimmed.length > 0) {
          // Check if this looks like a paragraph/section name that was split
          const combinedName = content + areaBTrimmed;
          if (/^[A-Z][A-Z0-9-]*\s*(SECTION\s*)?\.$/.test(combinedName.toUpperCase())) {
            // This is a paragraph or section name, combine without space
            content = combinedName;
          } else if (/[\s.]$/.test(areaA)) {
            // Area A ends with whitespace or period, add space
            content += ' ' + areaBTrimmed;
          } else if (areaA.endsWith('-') && /^[A-Z]/.test(areaBTrimmed)) {
            // Hyphenated name continues, don't add space
            content += areaBTrimmed;
          } else {
            // Word continues from Area A to Area B
            content += areaBTrimmed;
          }
        } else {
          content += areaBTrimmed;
        }
        content = content.trim();
        base.content = this.config.normalizeCase ? content.toUpperCase() : content;
        break;
      
      default:
        // Unknown indicator - treat as code but warn
        this.logger.warn(
          `Unknown indicator '${indicatorArea}' in column 7 at ${base.sourceFile}:${base.originalLineNumber}`
        );
        base.lineType = 'CODE';
        // Combine areas properly
        let unknownContent = areaA.trimEnd();
        const unknownAreaB = areaB.trimStart();
        
        if (unknownContent.length > 0 && unknownAreaB.length > 0) {
          if (/[\s\-.]$/.test(areaA)) {
            unknownContent += ' ' + unknownAreaB;
          } else {
            unknownContent += unknownAreaB;
          }
        } else {
          unknownContent += unknownAreaB;
        }
        unknownContent = unknownContent.trim();
        base.content = this.config.normalizeCase ? unknownContent.toUpperCase() : unknownContent;
        break;
    }

    return base;
  }

  /**
   * Process a line in free COBOL format
   */
  private processFreeFormatLine(rawLine: string, base: ProcessedLine): ProcessedLine {
    const trimmedLine = rawLine.trim();
    
    // Check for comment indicators at the start
    if (trimmedLine.startsWith('*') || trimmedLine.startsWith('/')) {
      base.lineType = 'COMMENT';
      base.content = trimmedLine.substring(1).trim();
      return base;
    }

    // Free format continuation is typically indicated by starting with certain characters
    // or ending the previous line with specific indicators
    // For now, treat all non-comment lines as code
    base.lineType = 'CODE';
    base.content = this.config.normalizeCase ? trimmedLine.toUpperCase() : trimmedLine;
    
    return base;
  }

  /**
   * Detect whether the source uses fixed or free format
   */
  private detectFormat(lines: string[]): CobolFormat {
    let fixedFormatIndicators = 0;
    let totalLinesChecked = 0;
    const maxLinesToCheck = Math.min(50, lines.length);

    for (let i = 0; i < maxLinesToCheck; i++) {
      const line = lines[i];
      if (!line || line.trim().length === 0) continue;

      totalLinesChecked++;

      // Check for fixed format indicators
      if (line.length > 6) {
        const indicator = line.charAt(6);
        if (indicator === '*' || indicator === '/' || indicator === '-' || indicator === ' ') {
          // Check if there's code content starting at column 8 or later
          const codeArea = line.substring(7);
          if (codeArea.trim().length > 0) {
            fixedFormatIndicators++;
          }
        }
      }

      // Check for sequence numbers in columns 1-6
      const sequenceArea = line.substring(0, 6).trim();
      if (/^\d+$/.test(sequenceArea)) {
        fixedFormatIndicators++;
      }
    }

    const fixedFormatRatio = totalLinesChecked > 0 ? fixedFormatIndicators / totalLinesChecked : 0;
    const isFixedFormat = fixedFormatRatio > 0.3; // If more than 30% of lines follow fixed format

    this.logger.verbose(
      `Format detection: ${fixedFormatIndicators}/${totalLinesChecked} lines suggest fixed format (${(fixedFormatRatio * 100).toFixed(1)}%)`
    );

    return {
      isFixedFormat,
      maxLineLength: isFixedFormat ? 72 : 255,
    };
  }

  /**
   * Check if a line should be continued with the next line
   */
  private isLineToBecontinued(_currentLine: ProcessedLine, nextRawLine?: string): boolean {
    if (!nextRawLine) return false;

    // In fixed format, continuation is indicated by '-' in column 7 of the NEXT line
    if (this.config.format.isFixedFormat) {
      const paddedNextLine = nextRawLine.padEnd(7, ' ');
      return paddedNextLine.length > 6 && paddedNextLine.charAt(6) === '-';
    }

    // Free format continuation rules would be different
    // For now, return false as this is more complex
    return false;
  }

  /**
   * Combine two lines where the second is a continuation of the first
   */
  private combineContinuationLines(
    mainLine: ProcessedLine,
    continuationLine: ProcessedLine
  ): ProcessedLine {
    return {
      ...mainLine,
      content: mainLine.content + ' ' + continuationLine.content,
      rawContent: mainLine.rawContent + '\n' + continuationLine.rawContent,
    };
  }

  /**
   * Extract PROCEDURE DIVISION content from processed lines
   */
  public extractProcedureDivision(lines: ProcessedLine[]): ProcessedLine[] {
    const procedureStart = this.findProcedureDivisionStart(lines);
    if (procedureStart === -1) {
      this.logger.warn('PROCEDURE DIVISION not found in source');
      return [];
    }

    const procedureEnd = this.findProcedureDivisionEnd(lines, procedureStart);
    const procedureLines = lines.slice(procedureStart, procedureEnd === -1 ? undefined : procedureEnd);

    this.logger.verbose(
      `Extracted ${procedureLines.length} lines from PROCEDURE DIVISION (starting at line ${lines[procedureStart]?.originalLineNumber})`
    );

    return procedureLines;
  }

  /**
   * Find the start of PROCEDURE DIVISION
   */
  private findProcedureDivisionStart(lines: ProcessedLine[]): number {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (line.lineType === 'CODE' && 
          line.content.includes('PROCEDURE') && 
          line.content.includes('DIVISION')) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Find the end of PROCEDURE DIVISION (start of next division or end of program)
   */
  private findProcedureDivisionEnd(lines: ProcessedLine[], startIndex: number): number {
    // Look for next division or end of program
    for (let i = startIndex + 1; i < lines.length; i++) {
      const line = lines[i];
      if (line.lineType === 'CODE') {
        // Check for other divisions
        if (line.content.includes('DIVISION') && 
            !line.content.includes('PROCEDURE')) {
          return i;
        }
        // Check for end of program
        if (line.content.includes('END PROGRAM') || 
            line.content.trim() === 'END' ||
            line.content.includes('STOP RUN')) {
          return i + 1;
        }
      }
    }
    return -1; // End of file
  }
}
