__all__ = ["FileHmap", "HmapRow"]

from .datafile import *
from ..misc import *
import logging

_logger = logging.getLogger(__name__)
_logger.addHandler(logging.NullHandler())

class HmapRow(AttrsPart):
    """Same structure as pfantlib.f90::hmap_row type."""

    attrs = ["fn", "na", "nb", "clam", "kiex", "c1"]

    def __init__(self):
        AttrsPart.__init__(self)
        self.fn = None
        self.na = None
        self.nb = None
        self.clam = None
        self.kiex = None
        self.c1 = None


class FileHmap(DataFile):
    """
    Represents hygrogen lines map file.

    Imitates the logic of reader_hmap.f90::read_hmap().

    Attributes match reader_hmap.f90::hmap_* (minus the "hmap_" prefix)
    """

    default_filename = "hmap.dat"
    attrs = ["rows"]

    def __init__(self):
        DataFile.__init__(self)

        # List of HmapRow objects
        self.rows = []

    def __len__(self):
        return len(self.rows)

    def _do_load(self, filename):
        """Loads from file."""

        with open(filename, "r") as h:
            for i, line in enumerate(h):
                try:
                    line = line.strip()
                    if line.startswith("#") or len(line) == 0:
                        continue

                    r = HmapRow()
                    [r.fn, r.na, r.nb, r.clam, r.kiex, r.c1] = line.split()
                    [r.na, r.nb] = map(int, (r.na, r.nb))
                    [r.clam, r.kiex, r.c1] = map(float, [r.clam, r.kiex, r.c1])

                    self.rows.append(r)
                except:
                    # _logger.error("Error reading row #%d, file \"%s\"" % (i+1, filename))
                    raise


    def _do_save_as(self, filename):
        with open(filename, "w") as h:
            write_lf(h, "# filename / niv inf / niv sup / central lambda / kiex / c1")
            for r in self.rows:
                write_lf(h, "%s %s %s %s %s %s" % (r.fn, r.na, r.nb, r.clam, r.kiex, r.c1))
