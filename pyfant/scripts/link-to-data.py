#!/usr/bin/python
"""
Creates symbolic links to PFANT data files to prevent copies of these data files.

A star is specified by three data files whose typical names are:
main.dat, abonds.dat, and dissoc.dat .

The other data files (atomic/molecular lines, partition function, etc.)
are star-independent, and this script is a proposed solution to keep you from
copying these files for every new case.

How it works: link-to-data.py will look inside a given directory and create
symbolic links to files *.dat and *.mod.

The following files will be skipped:
  - main files, e.g. main.dat
  - dissoc files, e.g., dissoc.dat
  - abonds files, e.g., abonds.dat
  - .mod files with a single model inside, e.g., modeles.mod
  - hydrogen lines files

This script works in two different modes:

a) it looks for files in a subdirectory of PFANT/data
   (default mode)
   Example:
   > link-to-data.py common
   (will look inside the directory PFANT/data/common)

b) looks for files in a directory specified ("-p" option)
   Examples:
   > link-to-data.py -p /home/user/pfant-common-data
   > link-to-data.py -p ../../pfant-common-data

Note: in Windows, must run as administrator.
"""
import argparse
from pyfant import *
import logging
import os.path
import sys
import glob


misc.logging_level = logging.INFO


def print_skipped(reason):
    """Standardized printing for when a file was skipped."""
    print "   ... SKIPPED (%s)." % reason


def symlink(source, link_name):
    """
    Creates symbolic link for either operating system.

    http://stackoverflow.com/questions/6260149/os-symlink-support-in-windows
    """
    os_symlink = getattr(os, "symlink", None)
    if callable(os_symlink):
        os_symlink(source, link_name)
    else:
        import ctypes
        csl = ctypes.windll.kernel32.CreateSymbolicLinkW
        csl.argtypes = (ctypes.c_wchar_p, ctypes.c_wchar_p, ctypes.c_uint32)
        csl.restype = ctypes.c_ubyte
        flags = 1 if os.path.isdir(source) else 0
        if csl(link_name, source, flags) == 0:
            raise ctypes.WinError()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=SmartFormatter
    )

    parser.add_argument('-p', '--path', action='store_true',
      help='system path mode')
    parser.add_argument('directory', type=str, nargs=1,
     help='name of directory (either a subdirectory of PFANT/data or the path '
          'to a valid system directory (see modes of operation)')

    args = parser.parse_args()
    # print args

    if args.path:
        dir_ = args.directory[0]
    else:
        dir_ = os.path.abspath(os.path.join(
         os.path.dirname(os.path.realpath(sys.argv[0])),
         '..', '..', 'data', args.directory[0]
         ))

    star_classes = [FileMain, FileDissoc, FileAbonds]

    print "Will look inside directory %s" % dir_

    # makes list of files to analyse
    types = ('*.dat', '*.mod')
    ff = []
    for type_ in types:
        ff.extend(glob.glob(os.path.join(dir_, type_)))

    for f in ff:
        name = os.path.split(f)[1]

        flag_skip = False
        print "Considering file '%s' ..." % name
        if os.path.isfile(name) and not os.path.islink(name):
            print_skipped("file exists in local directory")
            flag_skip = True
        else:
            obj = load_with_classes(f, [FileMain, FileAbonds, FileDissoc, FileToH])
            if obj is not None:
                print_skipped("detected type %s" % obj.__class__.__name__)
                flag_skip = True
            else:
                obj = load_with_classes(f, [FileMod])
                if obj is not None:
                    if len(obj) == 1:
                        print_skipped("%s of only one record" % obj.__class__.__name__)
                        flag_skip = True

        if not flag_skip:
            try:
                if os.path.islink(name):
                    os.remove(name)
                    s_action = "replaced existing"
                else:
                    s_action = "created"
                symlink(f, name)
                print "   ... %s link" % s_action
            except Exception as e:
                print_error("Error creating link: %s" % str(e))
