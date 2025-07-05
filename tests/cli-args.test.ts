import { validateCliArgs } from '../src/cli/args';
import { CliArgs } from '../src/types';

describe('CLI Args Validation', () => {
  describe('validateCliArgs', () => {
    it('should validate a correct CLI args object', () => {
      const args: CliArgs = {
        file: 'test.cob',
        verbose: false,
        format: 'json'
      };

      const result = validateCliArgs(args);
      expect(result.valid).toBe(true);
      expect(result.error).toBeUndefined();
    });

    it('should reject when file is missing', () => {
      const args: CliArgs = {
        file: '',
        verbose: false,
        format: 'json'
      };

      const result = validateCliArgs(args);
      expect(result.valid).toBe(false);
      expect(result.error).toBe('File path is required');
    });

    it('should reject invalid format', () => {
      const args: CliArgs = {
        file: 'test.cob',
        verbose: false,
        format: 'xml' as any
      };

      const result = validateCliArgs(args);
      expect(result.valid).toBe(false);
      expect(result.error).toBe('Format must be either "json" or "yaml"');
    });

    it('should accept yaml format', () => {
      const args: CliArgs = {
        file: 'test.cob',
        verbose: false,
        format: 'yaml'
      };

      const result = validateCliArgs(args);
      expect(result.valid).toBe(true);
      expect(result.error).toBeUndefined();
    });

    it('should accept optional parameters', () => {
      const args: CliArgs = {
        file: 'test.cob',
        verbose: true,
        format: 'json',
        output: 'output.json'
      };

      const result = validateCliArgs(args);
      expect(result.valid).toBe(true);
      expect(result.error).toBeUndefined();
    });
  });
});
