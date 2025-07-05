// Test file to demonstrate PERFORM statement analysis
import { Logger } from './src/utils/logger';
import { LineProcessor } from './src/parser/line-processor';
import { ProcedureDivisionParser } from './src/parser/procedure-division-parser';
import { CobolFormatDetector } from './src/utils/cobol-format-detector';

const testCobolCode = `IDENTIFICATION DIVISION.
PROGRAM-ID. PERFORM-TEST.

PROCEDURE DIVISION.

MAIN-SECTION SECTION.

MAIN-PARA.
    DISPLAY "Starting main".
    PERFORM SUB-PARA.
    PERFORM LOOP-PARA 5 TIMES.
    PERFORM CALC-SECTION.
    PERFORM START-PARA THROUGH END-PARA.
    DISPLAY "Main complete".

SUB-PARA.
    DISPLAY "In sub paragraph".
    PERFORM HELPER-PARA.

HELPER-PARA.
    DISPLAY "Helper routine".

CALC-SECTION SECTION.

CALC-PARA.
    DISPLAY "Calculating".
    PERFORM MATH-PARA UNTIL DONE = "Y".

MATH-PARA.
    DISPLAY "Math operation".

LOOP-SECTION SECTION.

START-PARA.
    DISPLAY "Start of range".

MIDDLE-PARA.
    DISPLAY "Middle of range".

END-PARA.
    DISPLAY "End of range".

LOOP-PARA.
    DISPLAY "Loop iteration".
`;

async function demonstratePerformAnalysis() {
  const logger = new Logger();
  logger.setVerbose(true);

  console.log('COBOL PERFORM Statement Analysis Demo');
  console.log('====================================\\n');

  // Process the lines
  const formatDetector = new CobolFormatDetector(logger);
  const lines = testCobolCode.split('\\n');
  const format = formatDetector.detectFormat(lines);
  
  const lineProcessor = new LineProcessor(logger, {
    format,
    preserveComments: true,
    preserveBlankLines: false,
    normalizeCase: true,
  });
  const processedLines = lineProcessor.processLines(lines, 'perform-test.cob');

  // Parse the PROCEDURE DIVISION
  const parser = new ProcedureDivisionParser(logger);
  const procedureDivision = parser.parseProcedureDivision(processedLines.lines);

  if (!procedureDivision) {
    console.log('No PROCEDURE DIVISION found!');
    return;
  }

  console.log(`Found ${procedureDivision.sections.length} sections and ${procedureDivision.paragraphs.length} standalone paragraphs\\n`);

  // Validate PERFORM targets
  const errors = parser.validatePerformTargets(procedureDivision);
  if (errors.length > 0) {
    console.log('PERFORM Validation Errors:');
    errors.forEach(error => console.log(`  - ${error}`));
    console.log();
  } else {
    console.log('All PERFORM targets validated successfully!\\n');
  }

  // Generate and display call graph report
  const callGraphReport = parser.generateCallGraphReport(procedureDivision);
  console.log(callGraphReport);

  // Show symbol table
  const symbolTable = parser.getSymbolTable();
  console.log(`Symbol Table (${symbolTable.size} entries):`);
  for (const [name, procedure] of symbolTable) {
    console.log(`  - ${name} (${procedure.type})`);
  }
}

// Export for potential use in tests
export { demonstratePerformAnalysis, testCobolCode };

// Run demo if this file is executed directly
if (require.main === module) {
  demonstratePerformAnalysis().catch(console.error);
}
