"""
Miscellanea routines that depend on other pyfant modules.

Rule: no pyfant module can import util!!!

"""
__all__ = ["run_parallel",
           "load_any_file", "load_spectrum",
            "setup_inputs", "link_to_data", "copy_star",
           "load_spectrum_fits_messed_x"]

# from pyfant.misc import *
from pyfant import *
import time
import traceback
import numpy as np
import copy
import os
import glob
import shutil
from astropy.io import fits

###############################################################################
# # Routines to load file of unknown format

# List of classe representing all file formats either read or written by
# Fortran.
#
# note: leave FileAbonds to the end because it is too general
_classes_txt = [FileAbsoru2, FileHmap, FileMain, FileDissoc,
                FileSpectrumNulbad, FileSpectrumPfant, FileToH, FileAbonds,
                FileSpectrumXY, FileAtoms, FileMolecules,
                FileOpa, FileModTxt]

_classes_bin = [FileModBin, FileSpectrumFits, FileMoo, FileWebsimCube, FileDCube, FileSpectrumList]

_classes_sp = [FileModBin, FileSpectrumNulbad, FileSpectrumPfant, FileSpectrumXY,
               FileSpectrumFits]

def load_any_file(filename):
    """
    Attempts to load filename by trial-and-error using _classes as list of classes.
    """

    # Splits attempts using ((binary X text) file) criterion
    if is_text_file(filename):
        return load_with_classes(filename, _classes_txt)
    else:
        return load_with_classes(filename, _classes_bin)

def load_spectrum(filename):
    """
    Attempts to load spectrum as one of the supported types. Returns a Spectrum, or None
    """
    f = load_with_classes(filename, _classes_sp)
    if f:
        return f.spectrum
    return None


def load_spectrum_fits_messed_x(filename, sp_ref=None):
    """Loads FITS file spectrum that does not have the proper headers. Returns a Spectrum"""

    # First tries to load as usual
    f = load_with_classes(filename, (FileSpectrumFits,))

    if f is None:
        hdul = fits.open(filename)

        hdu = hdul[0]
        if not hdu.header.get("CDELT1"):
            hdu.header["CDELT1"] = 1 if sp_ref is None else sp_ref.delta_lambda
        if not hdu.header.get("CRVAL1"):
            hdu.header["CRVAL1"] = 0 if sp_ref is None else sp_ref.x[0]

        ret = Spectrum()
        ret.from_hdu(hdu)
        ret.filename = filename
        original_shape = ret.y.shape  # Shape of data before squeeze
        # Squeezes to make data of shape e.g. (1, 1, 122) into (122,)
        ret.y = ret.y.squeeze()

        if len(ret.y.shape) > 1:
            raise RuntimeError(
                "Data contains more than 1 dimension (shape is %s), FITS file is not single spectrum" % (
                original_shape,))

    return ret


# ##################################################################################################
# Terminal-based interface

def run_parallel(rr, max_simultaneous=None, flag_console=True, runnable_manager=None):
    """
    Arguments:
      rr -- list of Runnable instances
      max_simultaneous -- (optional, default is RunnableManager default)
       maximum number of simultaneous processes.
      runnable_manager=None -- (optional) if passed, will use passed;
       if not, will create new.

    Returns: the RunnableManager object
    """
    # Adds to pool
    logger = get_python_logger()
    if runnable_manager:
        assert isinstance(runnable_manager, RunnableManager)
        rm = runnable_manager
    else:
        rm = RunnableManager(max_simultaneous=max_simultaneous)
    flag_had_to_start = False
    if not rm.flag_start_called:
        rm.start()
        flag_had_to_start = True

    rm.add_runnables(rr)

    # Primitive thread monitor
    if flag_console:
        while True:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
            print rm
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
            s = raw_input("[Enter] -- [e]xit & keep in loop -- [q]uit -- [k]ill running >>> ")
            if s.lower() == "q":
                if rm.is_alive():
                    try:
                        rm.exit()
                    except:
                        logger.exception("Error trying to exit")
                break
            if s.lower() == "e":
                try:
                    rm.exit()
                except:
                    logger.exception("Error trying to exit")
            if s.lower() == "k":
                rm.kill_runnables()
    else:
        rm.wait_until_finished()
        if flag_had_to_start:
            rm.exit()


    print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"+(" ALIVE" if rm.is_alive() else " DEAD")
    print "test-tm2 [SUPPOSED TO HAVE] EXITED"
    print rm

    return rm


def setup_inputs(dest_dir='.', star='sun-asplund-2009', common='common', h=True, atoms=True, molecules=True, opa=True):
    """
    Sets up input data for spectral synthesis.
    
    Arguments:
      dest_dir='.' -- directory where files and links will be created
      star='sun-asplund-2009' -- directory (relative to PFANT/data) for stellar data
      common='common' -- directory (relative to PFANT/data) for star-independent data files
      h=True -- whether to look for hmap.dat
      atoms=True -- whether to look for atoms.dat
      molecules=True -- whether to look for molecules.dat
      opa=True -- whether to look for grid.moo
    """
    
    logger = get_python_logger()    
    dd = get_data_dir()

    # Functions that return full path, given a filename, to ...
    fd = lambda filename: os.path.join(dest_dir, filename)  # ... Destination directory
    fs = lambda filename: os.path.join(dd, star, filename)  # ... Stellar data directory
    fc = lambda filename: os.path.join(dd, common, filename)  # ... Common data directory
     

    # ## main.dat not present
    if not os.path.isfile(fd("main.dat")):
        zz_mnbp = ["main.dat", "abonds.dat", "modeles.mod"]  # files that must not be present if main.dat is not present
        
        for z in zz_mnbp:
            if os.path.isfile(fd(z)):
                raise RuntimeError("Found file '%s' in local directory."
                 "If 'main.dat' is not present, files %s must also not exist." % zz_mnbp[1:])


    # ## Stellar data...
    zz = ["main.dat", "abonds.dat"]
    copy_or_skip_files([fs(z) for z in zz], dest_dir=dest_dir)

    # ## Common data...
    zz = ["absoru2.dat", "partit.dat", "grid.mod"]
    if opa: zz.append("grid.moo")
    if h: zz.append("hmap.dat")
    if atoms: zz.append("atoms.dat")
    if molecules: zz.append("molecules.dat")
    create_or_replace_or_skip_links([fc(z) for z in zz], dest_dir=dest_dir)


def copy_star(src_dir):
    star_classes = [FileMain, FileDissoc, FileAbonds]

    print "Will look inside directory %s" % src_dir

    # makes list of files to analyse
    types = ('*.dat', '*.mod')
    ff = []
    for type_ in types:
        ff.extend(glob.glob(os.path.join(src_dir, type_)))

    copy_or_skip_files(ff)
    

def link_to_data(src_dir):
    star_classes = [FileMain, FileDissoc, FileAbonds]

    print "Will look inside directory %s" % src_dir

    # makes list of files to analyse
    types = ('*.dat', '*.mod', '*.moo')
    ff = []
    for type_ in types:
        ff.extend(glob.glob(os.path.join(src_dir, type_)))
        
    create_or_replace_or_skip_links(ff)


def create_or_replace_or_skip_links(ff, dest_dir="."):
    """Creates a series of links given a list of target filepaths.
    
    Arguments:
      ff -- list of full path to files
      dest_dir="." -- destination directory
      
    It skips files of types FileMain, FileAbonds, FileDissoc, FileToH
    """
    for f in ff:
        name = os.path.split(f)[1]
        ptd = os.path.join(dest_dir, name)  # path to destination

        flag_skip = False
        print "Considering file '%s' ..." % name
        if os.path.isfile(ptd) and not os.path.islink(ptd):
            print_skipped("file exists in local directory")
            flag_skip = True
        else:
            obj = load_with_classes(f, [FileMain, FileAbonds, FileDissoc, FileToH])
            if obj is not None:
                print_skipped("detected type %s" % obj.__class__.__name__)
                flag_skip = True
            else:
                obj = load_with_classes(f, [FileModBin])
                if obj is not None:
                    if len(obj) == 1:
                        print_skipped("%s of only one record" % obj.__class__.__name__)
                        flag_skip = True

        if not flag_skip:
            try:
                if os.path.islink(ptd):
                    os.remove(ptd)
                    s_action = "replaced existing"
                else:
                    s_action = "created"
                symlink(f, ptd)
                print "   ... %s link" % s_action
            except Exception as e:
                print_error("Error creating link: %s" % str(e))



def copy_or_skip_files(ff, dest_dir="."):
    """Copies a series of files, skipping those which already exist.
    
    Arguments:
      ff -- list of full paths to files to be copied
      dest_dir="." -- destination directory
    """
      
    for f in ff:
        name = os.path.split(f)[1]

        flag_skip = False
        print "Considering file '%s' ..." % name
        if os.path.isfile(name):
            print_skipped("file exists in local directory")
            flag_skip = True
        else:
            obj = load_with_classes(f, [FileMain, FileAbonds, FileDissoc])
            if obj is not None:
                pass
            else:
                print_skipped("neither main, abonds, nor dissoc file")
                flag_skip = True

        if not flag_skip:
            try:
                shutil.copy(f, dest_dir)
                print "   ... file copied"
            except Exception as e:
                print_error("Error copying file: %s" % str(e))
