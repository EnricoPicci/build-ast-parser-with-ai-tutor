This is an experiment to build an AST parser for Cobol using the GitHub Copilot "agent" (with Claude Sonnet 4) as tutor.

# Basic requirements
- The parser should be able to parse Cobol source code and produce an Abstract Syntax Tree (AST).
- The AST should represent the structure of the Cobol code as a tree of nodes starting from the root node which is the program.
- Below the root node it there shoul nodes for the divisions. At first we are interested only in the PROCEDURE DIVISION.
- Below the PROCEDURE DIVISION node there should be nodes for the sections and paragraphs.
- Each section and paragraph node must have:
    - A name which is the name of the section or paragraph.
    - The list of source code lines that belong to the section or paragraph.
    - the list of paragraphs or sections that are called by the section or paragraph with the PERFORM statement.

# Technical requirements
- The parser should be implemented in Typescript and should run as a Node.js application
- The parser Node.js application expcts the path to the Cobol source code file as a command line argument.
- The parser should generated an AST in JSON format and save it to a file named as the name of the source code file with a postfix `ast.json` in the same directory as the source code file.