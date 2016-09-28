__all__ = ["FilePar"]

from .datafile import DataFile
from collections import OrderedDict

class FilePar(DataFile):
    """Represents a ".par" (Websim-Compass session specification) file in memory"""

    description = "Websim-Compass session parameters"
    default_filename = None
    attrs = ["params"]

    def __init__(self):
        DataFile.__init__(self)
        self.params = OrderedDict()


    def _do_load(self, filename):
        data = []  # keyword, value pairs create dict at the end
        with open(filename, "r") as f:
            for line in f:
                s = line.strip()
                if s.startswith("#"):
                    continue
                if len(s) == 0:
                    continue

                KEYWORD_LENGTH = 26
                keyword = s[:KEYWORD_LENGTH].strip()
                value = s[KEYWORD_LENGTH:]
                data.append((keyword, value))

        self.params = OrderedDict(data)

    def _do_save_as(self, filename):
        raise RuntimeError("Not applicable")

