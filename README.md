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

# COPY inclusion requirements
In Cobol the source code can include other source code files using the `COPY` statement. The parser should be able to handle this by:
- Finding the `COPY` statements in the source code. At the moment we are interested only in the `COPY` statements that are in the PROCEDURE DIVISION.
- Reading the content of the included files and replace the `COPY` statement with the content of the included file.
- The inclusion must be recursive, meaning that if the included file has its own `COPY` statements, those must be resolved as well.
- A link between each line of code and the file it comes from must be maintained in the AST. what I suggest is to model each line of code as a node in the AST with a reference to the file it comes from. This way we can keep track of the origin of each line of code in the AST. If you have better ideas, please let me know.

# Technical requirements
- The parser should be implemented in Typescript and should run as a Node.js application
- The parser Node.js application expcts the path to the Cobol source code file as a command line argument.
- The parser should generated an AST in JSON format and save it to a file named as the name of the source code file with a postfix `ast.json` in the same directory as the source code file.    