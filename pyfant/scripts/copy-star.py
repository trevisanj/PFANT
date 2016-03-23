#!/usr/bin/python
"""
Copies star-specific files (whose typical names are "main.dat", "abonds.dat", and "dissoc.dat") to local directory.

This script can work in three different modes:

a) default mode: it looks for files in a subdirectory of PFANT/data
   Example:
   > link-to-data.py common
   (will look inside the directory PFANT/data/common)

b) "-l" option: lists subdirectories of PFANT/data

c) "-p" option: looks for files in a directory specified:
   > link-to-data.py -p /home/user/pfant-common-data
   > link-to-data.py -p ../../pfant-common-data

Note: in Windows, this script must be run as administrator.
"""
import argparse
from pyfant import *
import logging
import os.path
import sys
import glob
import shutil

misc.logging_level = logging.INFO


def print_skipped(reason):
    """Standardized printing for when a file was skipped."""
    print "   ... SKIPPED (%s)." % reason



if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=SmartFormatter
    )
    parser.add_argument('-l', '--list', action='store_true',
      help='lists subdirectories of '+get_data_dir())
    parser.add_argument('-p', '--path', action='store_true',
      help='system path mode')
    parser.add_argument('directory', type=str, nargs="?",
     help='name of directory (either a subdirectory of PFANT/data or the path '
          'to a valid system directory (see modes of operation)')

    args = parser.parse_args()

    if (not args.directory or len(args.directory) ==  0) and not args.list:
        print "Directory name is required, except if '-l' option specified."
        parser.print_usage()
        sys.exit()

    if args.list:
        for dirname in get_data_subdirs():
            print dirname
        sys.exit()

    if args.path:
        dir_ = args.directory
    else:
        dir_ = os.path.join(get_data_dir(), args.directory)
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
                shutil.copy(f, ".")
                print "   ... file copied"
            except Exception as e:
                print_error("Error copying file: %s" % str(e))
