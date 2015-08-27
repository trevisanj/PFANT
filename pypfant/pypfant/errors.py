class ParseError(Exception):
  pass

class FileConsistencyError(Exception):
  """Things that must match within file don't."""
  pass

class FailedError(Exception):
  """Execution failed for one of the executables"""