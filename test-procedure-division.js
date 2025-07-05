const fs = require('fs');
const path = require('path');

// Simple test to check if we can find PROCEDURE DIVISION in real files
function testProcedureDivisionDetection() {
  const testFiles = [
    'test-data-real/cobol/example_1.PCO',
    'test-data-real/cobol/example_2.PCO'
  ];

  testFiles.forEach(file => {
    console.log(`\nTesting file: ${file}`);
    
    if (!fs.existsSync(file)) {
      console.log(`File not found: ${file}`);
      return;
    }

    const content = fs.readFileSync(file, 'utf8');
    const lines = content.split('\n');
    
    // Look for PROCEDURE DIVISION
    let found = false;
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim().toUpperCase();
      if (line.includes('PROCEDURE') && line.includes('DIVISION')) {
        console.log(`Found at line ${i + 1}: ${lines[i].trim()}`);
        found = true;
      }
    }
    
    if (!found) {
      console.log('PROCEDURE DIVISION not found');
    }
  });
}

testProcedureDivisionDetection();
