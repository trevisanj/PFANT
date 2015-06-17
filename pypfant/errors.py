class ParseError(Exception):
    pass

class FileConsistencyError(Exception):
    """Things that must match within file don't."""
    pass