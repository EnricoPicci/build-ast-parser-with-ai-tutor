#!/usr/bin/env node

import { parseCliArgs, validateCliArgs } from './cli/args';
import { Logger, FileHandler } from './utils';
import { ASTBuilder } from './parser/ast-builder';

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

    // Initialize AST Builder
    const astBuilder = new ASTBuilder(logger);

    // Build AST from COBOL file
    const result = await astBuilder.buildAST(args.file);

    // Handle results
    if (result.success && result.ast) {
      logger.success(`Successfully built AST for: ${args.file}`);
      
      // Log statistics
      if (result.stats) {
        const stats = result.stats;
        logger.info(`Processing completed in ${stats.processingTime}ms`);
        logger.info(`Total lines: ${stats.totalLines}`);
        logger.info(`Procedure division lines: ${stats.procedureLines}`);
        logger.info(`Sections: ${stats.sectionCount}, Paragraphs: ${stats.paragraphCount}`);
        logger.info(`PERFORM statements: ${stats.performCount}`);
        
        if (stats.errorCount > 0) {
          logger.warn(`Validation errors: ${stats.errorCount}`);
        }
        if (stats.warningCount > 0) {
          logger.warn(`Validation warnings: ${stats.warningCount}`);
        }
      }

      // Determine output file path
      const outputPath = args.output || astBuilder.generateOutputFilename(args.file);
      
      // Save AST to file
      await astBuilder.saveASTToFile(
        result.ast,
        outputPath,
        true, // Include validation info
        result.validationIssues,
        result.stats
      );
      
      logger.success(`AST saved to: ${outputPath}`);

      // Display validation issues if any
      if (result.validationIssues && result.validationIssues.length > 0) {
        logger.info('\nValidation Issues:');
        for (const issue of result.validationIssues) {
          const prefix = issue.type === 'ERROR' ? 'ERROR' : 'WARN';
          const location = issue.lineNumber ? ` (line ${issue.lineNumber})` : '';
          logger.info(`  ${prefix}: ${issue.message}${location}`);
        }
      }

      // If output to stdout was requested and no output file specified
      if (!args.output) {
        // Only show summary to stdout since we already saved to file
        const summary = {
          success: true,
          file: args.file,
          outputFile: outputPath,
          summary: {
            sections: result.stats?.sectionCount || 0,
            paragraphs: result.stats?.paragraphCount || 0,
            performStatements: result.stats?.performCount || 0,
            validationErrors: result.stats?.errorCount || 0,
            validationWarnings: result.stats?.warningCount || 0,
          },
        };

        const outputStr = args.format === 'yaml'
          ? formatAsYaml(summary)
          : JSON.stringify(summary, null, 2);
        
        console.log(outputStr);
      }
    } else {
      logger.error(`Failed to build AST: ${result.error}`);
      
      // Still try to save partial results if we have statistics
      if (result.stats) {
        const errorSummary = {
          success: false,
          file: args.file,
          error: result.error,
          stats: result.stats,
        };
        
        const outputStr = args.format === 'yaml'
          ? formatAsYaml(errorSummary)
          : JSON.stringify(errorSummary, null, 2);
          
        if (args.output) {
          const fileHandler = new FileHandler(logger);
          await fileHandler.writeFile(args.output, outputStr);
          logger.info(`Error report written to: ${args.output}`);
        } else {
          console.log(outputStr);
        }
      }
      
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
