import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';
import { CliArgs } from '../types';

/**
 * Parse command line arguments
 */
export function parseCliArgs(): CliArgs {
  const argv = yargs(hideBin(process.argv))
    .usage('Usage: $0 <file> [options]')
    .command('$0 <file>', 'Parse a COBOL file and generate AST', yargs => {
      yargs.positional('file', {
        describe: 'Path to the COBOL file to parse',
        type: 'string',
        demandOption: true,
      });
    })
    .option('verbose', {
      alias: 'v',
      type: 'boolean',
      description: 'Enable verbose output',
      default: false,
    })
    .option('format', {
      alias: 'f',
      type: 'string',
      choices: ['json', 'yaml'],
      description: 'Output format',
      default: 'json',
    })
    .option('output', {
      alias: 'o',
      type: 'string',
      description: 'Output file path (defaults to stdout)',
    })
    .help('h')
    .alias('h', 'help')
    .version('1.0.0')
    .alias('version', 'V')
    .example('$0 program.cob', 'Parse program.cob and output JSON to stdout')
    .example(
      '$0 program.cob -v -o ast.json',
      'Parse program.cob with verbose output and save to ast.json'
    )
    .example(
      '$0 program.cob -f yaml',
      'Parse program.cob and output YAML to stdout'
    )
    .strict()
    .parseSync();

  return {
    file: argv.file as string,
    verbose: argv.verbose,
    format: argv.format as 'json' | 'yaml',
    output: argv.output as string | undefined,
    help: Boolean(argv.help),
    version: Boolean(argv.version),
  };
}

/**
 * Validate CLI arguments
 */
export function validateCliArgs(args: CliArgs): {
  valid: boolean;
  error?: string;
} {
  if (!args.file) {
    return { valid: false, error: 'File path is required' };
  }

  if (args.format && !['json', 'yaml'].includes(args.format)) {
    return { valid: false, error: 'Format must be either "json" or "yaml"' };
  }

  return { valid: true };
}
