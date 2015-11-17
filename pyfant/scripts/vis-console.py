#!/usr/bin/python

"""
Text-based menu application to open and visualize data files.
"""

from pyfant import *
import os

def console():
    """Allows navigation & file loading & visualization."""

    objs = []
    idx = 0
    flag_set_idx = False

    while True:
        if len(objs) == 0:
            print "\n*** No objects loaded ***"
        else:
            print "\n*** Loaded objects ***"
            print "**********************"
            mac, maf = 0, 0
            for obj in objs:
               mac, maf = max(mac, len(obj.__class__.__name__)), max(maf, len(obj.filename))
            fmt = "*** %-2s %2d * %-"+str(maf)+"s * %-"+str(mac)+"s"
            for i, obj in enumerate(objs):
                print fmt % ("->" if idx == i else "", i+1, obj.filename, obj.__class__.__name__)
        opt = menu("MAIN MENU", ["List directory", "Change directory", "Load file",
                                 "Select file", "Visualization options"], cancel_label="Exit", flag_allow_empty=False)
        if opt == 0:
            break
        elif opt == 1:
            _, dd, ff = next(os.walk('.'))
            dd.sort()
            dd.insert(0, '..')
            dd.insert(0, '.')
            ff.sort()
            for d in dd:
                print '[%s]' % d
            for f in ff:
                print f
        elif opt == 2:
            x = raw_input("cd ")
            if len(x) > 0:
                try:
                    os.chdir(x)
                except OSError, e:
                    print e
        elif opt == 3:
            x = raw_input("filename: ")
            if len(x) > 0:
                try:
                    temp = load_any_file(x)
                    if temp is not None:
                        objs.append(temp)
                    else:
                        print_error("File was not recognized")
                    if not flag_set_idx:
                        # If not explicitly set selected object, it will be always the tail object
                        idx = len(objs)-1

                except (OSError, IOError), e:
                    print_error(e)
        elif opt == 4:
            if len(objs) == 0:
                print_error("No file loaded")
            elif len(objs) == 1:
                print_error("Only one file loaded")
            else:
                x = raw_input("Enter i (1 <= i <= %d)" % len(objs))
                try:
                    i = int(x)
                    if 1 <= i <= len(objs):
                        idx = i-1
                    else:
                        print_error("Out of range")
                except:
                    print_error("Invalid integer")
        elif opt == 5:
            if len(objs) == 0:
                print_error("No file loaded")
            else:
                show_menu(objs[idx])


if __name__ == "__main__":
    console()
