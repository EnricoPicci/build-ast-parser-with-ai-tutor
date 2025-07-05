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
