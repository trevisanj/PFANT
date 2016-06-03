#!/usr/bin/python
"""
Copies stellar data files (such as main.dat, abonds.dat, dissoc.dat) to local directory.

examples of usage:
  > copy-star.py
  (displays menu)

  > copy-star.py arcturus
  ("arcturus" is the name of a subdirectory of PFANT/data)

  > copy-star.py -p /home/user/pfant-common-data
  (use option "-p" to specify path)

  > copy-star.py -l
  (lists subdirectories of PFANT/data , doesn't copy anything)

"""
import argparse
from pyfant import *
import logging
import os.path
import sys
import glob
import shutil

misc.logging_level = logging.INFO


if __name__ == "__main__":
    flag_menu = len(sys.argv) == 1  # will display menu if no command-line arguments

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


    # "-l" mode
    if args.list:
        print fmt_ascii_h1("Subdirectories of '%s" % get_data_dir())
        for dirname in get_data_subdirs():
            print dirname
        sys.exit()
        
    # figures out the path to directory (dir_)
    if flag_menu:
        dirnames = get_star_data_subdirs()
        choice = menu("Choose a star", [x.capitalize() for x in dirnames],
                      cancel_label="quit", flag_allow_empty=True, flag_cancel=False)
        if choice <= 0:
            sys.exit()
        dir_ = os.path.join(get_data_dir(), dirnames[choice-1])
    else:
        if args.path:
            dir_ = args.directory
        else:
            dir_ = os.path.join(get_data_dir(), args.directory)
            
    if not os.path.isdir(dir_):
        print "'%s' is not a directory" % dir_
        sys.exit(-1)
    copy_star(dir_)
