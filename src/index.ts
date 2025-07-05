#!/usr/bin/env node

import { parseCliArgs, validateCliArgs } from './cli/args';
import { Logger, FileHandler } from './utils';
import { ParseResult } from './types';

/**
 * Main entry point for the COBOL AST Parser
 */
async function main(): Promise<void> {
  try {
    // Parse command line arguments
    const args = parseCliArgs();

    // Initialize logger with verbose setting
    const logger = new Logger(args.verbose);

    // Validate arguments
    const validation = validateCliArgs(args);
    if (!validation.valid) {
      logger.error(validation.error || 'Invalid arguments');
      process.exit(1);
    }

    logger.info('COBOL AST Parser starting...');
    logger.verbose(`Parsing file: ${args.file}`);
    logger.verbose(`Output format: ${args.format}`);
    if (args.output) {
      logger.verbose(`Output file: ${args.output}`);
    }

    // Initialize file handler
    const fileHandler = new FileHandler(logger);

    // Read and process the COBOL file
    const result = await processCobolFile(args.file, fileHandler, logger);

    // Output results
    if (result.success) {
      logger.success(`Successfully processed COBOL file: ${args.file}`);
      if (result.fileSize) {
        logger.info(`File size: ${result.fileSize} bytes`);
      }
      if (result.lineCount) {
        logger.info(`Lines processed: ${result.lineCount}`);
      }

      // For now, just output a basic success message
      // In future phases, this will output the actual AST
      const output = {
        success: true,
        file: result.filePath,
        message: 'File successfully read and validated',
        stats: {
          size: result.fileSize,
          lines: result.lineCount,
        },
      };

      const outputStr =
        args.format === 'yaml'
          ? formatAsYaml(output)
          : JSON.stringify(output, null, 2);

      if (args.output) {
        await fileHandler.writeFile(args.output, outputStr);
        logger.success(`Results written to: ${args.output}`);
      } else {
        console.log(outputStr);
      }
    } else {
      logger.error(`Failed to process COBOL file: ${result.error}`);
      process.exit(1);
    }
  } catch (error) {
    const logger = new Logger(true); // Enable verbose for error reporting
    logger.error(
      `Unexpected error: ${error instanceof Error ? error.message : 'Unknown error'}`
    );
    process.exit(1);
  }
}

/**
 * Process a COBOL file
 */
async function processCobolFile(
  filePath: string,
  fileHandler: FileHandler,
  logger: Logger
): Promise<ParseResult> {
  try {
    // Read the file
    const content = await fileHandler.readCobolFile(filePath);

    // Get file statistics
    const stats = await fileHandler.getFileStats(filePath);

    logger.verbose(
      `File content preview (first 100 chars): ${content.substring(0, 100)}...`
    );

    return {
      success: true,
      filePath: filePath,
      fileSize: stats.size,
      lineCount: stats.lines,
    };
  } catch (error) {
    return {
      success: false,
      filePath: filePath,
      error: error instanceof Error ? error.message : 'Unknown error',
    };
  }
}

/**
 * Simple YAML formatter (basic implementation)
 */
function formatAsYaml(obj: any): string {
  // This is a very basic YAML formatter
  // In a real implementation, you'd use a proper YAML library
  return Object.entries(obj)
    .map(([key, value]) => {
      if (typeof value === 'object' && value !== null) {
        const nested = Object.entries(value)
          .map(([k, v]) => `  ${k}: ${v}`)
          .join('\n');
        return `${key}:\n${nested}`;
      }
      return `${key}: ${value}`;
    })
    .join('\n');
}

// Run the main function
if (require.main === module) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}
