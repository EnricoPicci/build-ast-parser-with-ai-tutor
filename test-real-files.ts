import { LineProcessor } from './src/parser/line-processor';
import { Logger } from './src/utils/logger';
import { FileHandler } from './src/utils/file-handler';

async function testLineProcessorOnRealFiles() {
  const logger = new Logger(true); // verbose mode
  const fileHandler = new FileHandler(logger);
  const lineProcessor = new LineProcessor(logger, { normalizeCase: true });

  const testFiles = [
    'test-data-real/cobol/example_1.PCO',
    'test-data-real/cobol/example_2.PCO'
  ];

  for (const file of testFiles) {
    console.log(`\nTesting file: ${file}`);
    
    try {
      const content = await fileHandler.readCobolFile(file);
      const rawLines = content.split('\n');
      console.log(`Read ${rawLines.length} lines from ${file}`);

      const result = lineProcessor.processLines(rawLines, file);
      console.log(`Processed ${result.processedLineCount} lines (${result.commentLineCount} comments, ${result.continuationLineCount} continuations)`);
      console.log(`Detected format: ${result.detectedFormat.isFixedFormat ? 'FIXED' : 'FREE'}`);

      // Test PROCEDURE DIVISION extraction
      const procedureLines = lineProcessor.extractProcedureDivision(result.lines);
      console.log(`Extracted ${procedureLines.length} lines from PROCEDURE DIVISION`);
      
      if (procedureLines.length > 0) {
        console.log(`First few PROCEDURE DIVISION lines:`);
        procedureLines.slice(0, 5).forEach((line, index) => {
          console.log(`  ${index + 1}: (line ${line.originalLineNumber}) ${line.content}`);
        });
      } else {
        console.log('❌ No PROCEDURE DIVISION lines extracted');
        
        // Debug: Look for lines containing PROCEDURE and DIVISION
        const procedureDebug = result.lines.filter(line => 
          line.lineType === 'CODE' && 
          line.content.toUpperCase().includes('PROCEDURE') && 
          line.content.toUpperCase().includes('DIVISION')
        );
        
        if (procedureDebug.length > 0) {
          console.log('Found these lines with PROCEDURE and DIVISION:');
          procedureDebug.forEach(line => {
            console.log(`  Line ${line.originalLineNumber}: "${line.content}" (type: ${line.lineType})`);
          });
        }
      }
    } catch (error) {
      console.error(`Error processing ${file}:`, error);
    }
  }
}

testLineProcessorOnRealFiles().catch(console.error);
