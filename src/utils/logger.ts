/**
 * Simple logging utility for the COBOL parser
 */
export class Logger {
  private isVerbose: boolean;

  constructor(verbose: boolean = false) {
    this.isVerbose = verbose;
  }

  /**
   * Log an info message
   */
  info(message: string): void {
    console.log(`[INFO] ${message}`);
  }

  /**
   * Log a verbose message (only shown if verbose mode is enabled)
   */
  verbose(message: string): void {
    if (this.isVerbose) {
      console.log(`[VERBOSE] ${message}`);
    }
  }

  /**
   * Log an error message
   */
  error(message: string): void {
    console.error(`[ERROR] ${message}`);
  }

  /**
   * Log a warning message
   */
  warn(message: string): void {
    console.warn(`[WARN] ${message}`);
  }

  /**
   * Log a success message
   */
  success(message: string): void {
    console.log(`[SUCCESS] ${message}`);
  }

  /**
   * Set verbose mode
   */
  setVerbose(verbose: boolean): void {
    this.isVerbose = verbose;
  }
}
