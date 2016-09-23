__all__ = ["FileFits"]

from .datafile import DataFile
from astropy.io import fits
import os


class FileFits(DataFile):
    """
    Generic representation of a FITS file

    **Note** normally, the DataFile classes load operation reads all contents
    and closes the file, however this class keeps the file **open** as an
    astropy.io.fits.HDUList object in self.hdulist, because apparently many
    HDUList methods need the file open to work, even after calling
    HDUList.readall()
    """

    def __init__(self):
        DataFile.__init__(self)
        self.hdulist = None

    def _do_load(self, filename):
        self.hdulist = fits.open(filename)
        self.filename = filename

    def _do_save_as(self, filename):
        """Saves HDU list to FITS file."""
        if os.path.isfile(filename):
            os.unlink(filename)  # PyFITS does not overwrite file
        self.hdulist.writeto(filename)
