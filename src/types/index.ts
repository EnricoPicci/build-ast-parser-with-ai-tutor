/**
 * Configuration options for the COBOL parser
 */
export interface ParserConfig {
  /** Path to the COBOL file to parse */
  filePath: string;
  /** Enable verbose logging */
  verbose?: boolean;
  /** Output format for the AST */
  outputFormat?: 'json' | 'yaml';
  /** Output file path (optional, defaults to stdout) */
  outputPath?: string;
}

/**
 * Result of a parsing operation
 */
export interface ParseResult {
  /** Indicates if parsing was successful */
  success: boolean;
  /** Error message if parsing failed */
  error?: string;
  /** Path to the parsed file */
  filePath: string;
  /** File size in bytes */
  fileSize?: number;
  /** Number of lines processed */
  lineCount?: number;
}

/**
 * CLI argument interface
 */
export interface CliArgs {
  /** Input file path */
  file: string;
  /** Enable verbose output */
  verbose?: boolean;
  /** Output format */
  format?: 'json' | 'yaml';
  /** Output file path */
  output?: string | undefined;
  /** Show help */
  help?: boolean;
  /** Show version */
  version?: boolean;
}

/**
 * Base interface for all AST nodes
 */
export interface ASTNode {
  type: string;
  children: ASTNode[];
}

/**
 * Represents a processed COBOL source line with metadata
 */
export interface ProcessedLine {
  /** The content of the line after processing */
  content: string;
  /** Original line number (1-based) */
  originalLineNumber: number;
  /** Source file path */
  sourceFile: string;
  /** Whether this line was from a COPY file */
  isFromCopy: boolean;
  /** Type of line */
  lineType: 'COMMENT' | 'CONTINUATION' | 'CODE' | 'BLANK';
  /** Original raw content before processing */
  rawContent: string;
  /** Column where the indicator area content starts (usually column 7) */
  indicatorArea?: string;
  /** Area A content (columns 8-11 in fixed format) */
  areaA?: string;
  /** Area B content (columns 12-72 in fixed format) */
  areaB?: string;
}

/**
 * Represents COBOL format information
 */
export interface CobolFormat {
  /** Whether this is fixed format (true) or free format (false) */
  isFixedFormat: boolean;
  /** Maximum line length to consider */
  maxLineLength: number;
}

/**
 * Configuration for line processing
 */
export interface LineProcessorConfig {
  /** COBOL format settings */
  format: CobolFormat;
  /** Whether to preserve comments in output */
  preserveComments: boolean;
  /** Whether to preserve blank lines */
  preserveBlankLines: boolean;
  /** Whether to normalize case (convert to uppercase) */
  normalizeCase: boolean;
}

/**
 * Result of line processing operation
 */
export interface LineProcessingResult {
  /** Array of processed lines */
  lines: ProcessedLine[];
  /** Original line count */
  originalLineCount: number;
  /** Processed line count */
  processedLineCount: number;
  /** Number of comment lines found */
  commentLineCount: number;
  /** Number of continuation lines found */
  continuationLineCount: number;
  /** Format detected */
  detectedFormat: CobolFormat;
}

/**
 * Program node representing the root of the AST
 */
export interface ProgramNode extends ASTNode {
  type: 'PROGRAM';
  name: string;
  procedureDivision?: ProcedureDivisionNode;
  sourceLines: ProcessedLine[];
}

/**
 * Procedure Division node
 */
export interface ProcedureDivisionNode extends ASTNode {
  type: 'PROCEDURE_DIVISION';
  sections: SectionNode[];
  paragraphs: ParagraphNode[];
  sourceLines: ProcessedLine[];
}

/**
 * Section node
 */
export interface SectionNode extends ASTNode {
  type: 'SECTION';
  name: string;
  paragraphs: ParagraphNode[];
  sourceLines: ProcessedLine[];
  performedBy: PerformReference[];
  performs: PerformReference[];
}

/**
 * Paragraph node
 */
export interface ParagraphNode extends ASTNode {
  type: 'PARAGRAPH';
  name: string;
  sourceLines: ProcessedLine[];
  performedBy: PerformReference[];
  performs: PerformReference[];
}

/**
 * PERFORM statement reference
 */
export interface PerformReference {
  /** Name of the target being performed */
  targetName: string;
  /** Type of PERFORM statement */
  performType: 'SIMPLE' | 'THROUGH' | 'TIMES' | 'UNTIL' | 'VARYING' | 'INLINE';
  /** Source line where the PERFORM was found */
  sourceLine: ProcessedLine;
  /** Target end name for THROUGH statements */
  throughTarget?: string;
}
