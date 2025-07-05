import { promises as fs } from 'fs';
import { resolve, extname } from 'path';
import { Logger } from './logger';

/**
 * File I/O utility for reading COBOL files
 */
export class FileHandler {
  private logger: Logger;

  constructor(logger: Logger) {
    this.logger = logger;
  }

  /**
   * Check if a file exists and is readable
   */
  async fileExists(filePath: string): Promise<boolean> {
    try {
      await fs.access(filePath, fs.constants.R_OK);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Validate that the file path points to a COBOL file
   */
  isCobolFile(filePath: string): boolean {
    const ext = extname(filePath).toLowerCase();
    const cobolExtensions = ['.cob', '.cbl', '.pco', '.cobol'];
    return cobolExtensions.includes(ext);
  }

  /**
   * Read a COBOL file and return its contents
   */
  async readCobolFile(filePath: string): Promise<string> {
    const resolvedPath = resolve(filePath);

    this.logger.verbose(`Attempting to read file: ${resolvedPath}`);

    // Check if file exists
    if (!(await this.fileExists(resolvedPath))) {
      throw new Error(`File not found: ${resolvedPath}`);
    }

    // Check if it's a COBOL file
    if (!this.isCobolFile(resolvedPath)) {
      this.logger.warn(
        `File does not have a standard COBOL extension: ${resolvedPath}`
      );
    }

    try {
      const content = await fs.readFile(resolvedPath, 'utf-8');
      this.logger.verbose(
        `Successfully read ${content.length} characters from ${resolvedPath}`
      );
      return content;
    } catch (error) {
      throw new Error(
        `Failed to read file ${resolvedPath}: ${error instanceof Error ? error.message : 'Unknown error'}`
      );
    }
  }

  /**
   * Get file statistics
   */
  async getFileStats(
    filePath: string
  ): Promise<{ size: number; lines: number }> {
    const resolvedPath = resolve(filePath);

    try {
      const stats = await fs.stat(resolvedPath);
      const content = await this.readCobolFile(resolvedPath);
      const lines = content.split('\n').length;

      return {
        size: stats.size,
        lines: lines,
      };
    } catch (error) {
      throw new Error(
        `Failed to get file stats for ${resolvedPath}: ${error instanceof Error ? error.message : 'Unknown error'}`
      );
    }
  }

  /**
   * Write content to a file
   */
  async writeFile(filePath: string, content: string): Promise<void> {
    const resolvedPath = resolve(filePath);

    try {
      await fs.writeFile(resolvedPath, content, 'utf-8');
      this.logger.verbose(`Successfully wrote content to ${resolvedPath}`);
    } catch (error) {
      throw new Error(
        `Failed to write file ${resolvedPath}: ${error instanceof Error ? error.message : 'Unknown error'}`
      );
    }
  }
}
