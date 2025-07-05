import { Logger } from '../src/utils/logger';

describe('Logger', () => {
  let mockConsole: {
    log: jest.SpyInstance;
    error: jest.SpyInstance;
    warn: jest.SpyInstance;
  };

  beforeEach(() => {
    mockConsole = {
      log: jest.spyOn(console, 'log').mockImplementation(),
      error: jest.spyOn(console, 'error').mockImplementation(),
      warn: jest.spyOn(console, 'warn').mockImplementation(),
    };
  });

  afterEach(() => {
    mockConsole.log.mockRestore();
    mockConsole.error.mockRestore();
    mockConsole.warn.mockRestore();
  });

  describe('info', () => {
    it('should log info messages', () => {
      const logger = new Logger();
      logger.info('test message');
      expect(mockConsole.log).toHaveBeenCalledWith('[INFO] test message');
    });
  });

  describe('error', () => {
    it('should log error messages', () => {
      const logger = new Logger();
      logger.error('error message');
      expect(mockConsole.error).toHaveBeenCalledWith('[ERROR] error message');
    });
  });

  describe('verbose', () => {
    it('should not log verbose messages when verbose mode is disabled', () => {
      const logger = new Logger(false);
      logger.verbose('verbose message');
      expect(mockConsole.log).not.toHaveBeenCalled();
    });

    it('should log verbose messages when verbose mode is enabled', () => {
      const logger = new Logger(true);
      logger.verbose('verbose message');
      expect(mockConsole.log).toHaveBeenCalledWith('[VERBOSE] verbose message');
    });

    it('should log verbose messages after enabling verbose mode', () => {
      const logger = new Logger(false);
      logger.setVerbose(true);
      logger.verbose('verbose message');
      expect(mockConsole.log).toHaveBeenCalledWith('[VERBOSE] verbose message');
    });
  });

  describe('warn', () => {
    it('should log warning messages', () => {
      const logger = new Logger();
      logger.warn('warning message');
      expect(mockConsole.warn).toHaveBeenCalledWith('[WARN] warning message');
    });
  });

  describe('success', () => {
    it('should log success messages', () => {
      const logger = new Logger();
      logger.success('success message');
      expect(mockConsole.log).toHaveBeenCalledWith('[SUCCESS] success message');
    });
  });
});
