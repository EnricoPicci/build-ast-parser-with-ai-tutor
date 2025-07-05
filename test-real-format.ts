// Test script to verify LineProcessor handles the specific format from the real COBOL files
import { LineProcessor } from './src/parser/line-processor';
import { Logger } from './src/utils/logger';

const logger = new Logger(false);
const processor = new LineProcessor(logger);

// Test data based on the exact format from example_1.PCO and example_2.PCO
const rawLines = [
  '000100 DATA DIVISION.',
  '001689 PROCEDURE DIVISION USING APP-CMS01',
  '001690                                APP-CMS09',
  '001691                                DATI-TABELLA1',
  '001692                                DATI-TABELLA2.',
  '001693*--------------------*',
  '001694 L000-INIZIO-MAINLINE.',
  '001695*--------------------*',
  '001696     PERFORM  L100-HOUSEKEEPING.',
  '001697     PERFORM  L200-ELABORAZIONE.',
];

console.log('Processing lines...');
const result = processor.processLines(rawLines, 'test-real-format.cbl');

console.log(`\nProcessed ${result.processedLineCount} lines`);
console.log(`Format detected: ${result.detectedFormat.isFixedFormat ? 'Fixed' : 'Free'}`);
console.log(`Comment lines: ${result.commentLineCount}`);
console.log(`Continuation lines: ${result.continuationLineCount}`);

console.log('\nExtracting PROCEDURE DIVISION...');
const procedureLines = processor.extractProcedureDivision(result.lines);

console.log(`\nExtracted ${procedureLines.length} PROCEDURE DIVISION lines:`);
procedureLines.forEach((line, index) => {
  console.log(`${index + 1}. [${line.originalLineNumber}] ${line.content}`);
});

// Verify specific expectations
const firstLine = procedureLines[0];
if (firstLine && firstLine.content.includes('PROCEDURE DIVISION USING')) {
  console.log('\n✅ SUCCESS: First line correctly contains PROCEDURE DIVISION USING');
} else {
  console.log('\n❌ FAIL: First line does not contain expected PROCEDURE DIVISION USING');
}

const hasAppCms09 = procedureLines.some(line => line.content.includes('APP-CMS09'));
if (hasAppCms09) {
  console.log('✅ SUCCESS: Found APP-CMS09 parameter');
} else {
  console.log('❌ FAIL: APP-CMS09 parameter not found');
}

const hasMainline = procedureLines.some(line => line.content.includes('L000-INIZIO-MAINLINE'));
if (hasMainline) {
  console.log('✅ SUCCESS: Found main section L000-INIZIO-MAINLINE');
} else {
  console.log('❌ FAIL: Main section L000-INIZIO-MAINLINE not found');
}
