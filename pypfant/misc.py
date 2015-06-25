
__all__ = ["str_vector", "float_vector", "path_to_default", "new_filename", "str2bool",
           "write_lf", "bool2str", "list2str", "menu"]

import os.path

# reads next line of file and makes it a vector of strings
str_vector = lambda f: f.readline().split()

# reads next line of file and makes it a vector of floats
float_vector = lambda f: [float(s) for s in str_vector(f)]


def path_to_default(fn):
    """Returns full path to default data file."""
    p = os.path.join(os.path.dirname(os.path.realpath(__file__)), "data", "default", fn)
    return p


def new_filename(prefix, extension=""):
    """Returns a file name that does not exist yet, e.g. prefix.0001.extension"""
    i = 0
    while True:
        ret = '%s.%04d.%s' % (prefix, i, extension) \
            if extension else '%s.%04d' % (prefix, i)
        if not os.path.exists(ret):
            break
        i += 1
        return ret


def str2bool(s):
    """Understands "T"/"F" only (case-sensitive). To be used for file parsing."""
    if s == "T":
        return True
    elif s == "F":
        return False
    raise ValueError("I don't understand '%s' as a logical value" % s)


def bool2str(x):
    """Converts bool variable to either "T" or "F"."""
    assert isinstance(x, bool)
    return "T" if x else "F"


def list2str(l):
    """Converts list to string without the brackets."""
    return " ".join([str(x) for x in l])


def write_lf(h, s):
    """Adds lf to end of string and writes it to file."""
    h.write(s+"\n")


def menu(title, options, cancel_label="Cancel", flag_allow_empty=False):
    """Text menu.

    Arguments:
      title
      options -- sequence of strings
      cancel_label='Cancel' -- label to show at last "zero" option
      flag_allow_empty=0 -- Whether to allow empty option

    Returns:
      option -- an integer: None; 0-Back/Cancel/etc; 1, 2, ...

    Adapted from irootlab menu.m
    """

    no_options, flag_ok, ch, lt = len(options), 0, 'p', len(title)
    option = None  # result

    while True:
        print ""
        print "  "+ch*(lt+8)
        print "  "+ch*3+" "+title+" "+ch*3
        print "  "+ch*(lt+8)
        for i, s in enumerate(options):
            print "  %d - %s" % (i+1, s)
        print "  0 - << (*%s*)" % cancel_label
        s_option = raw_input('? ')

        n_try = 0
        while True:
            if n_try >= 10:
                print 'You are messing up!'
                break

            if len(s_option) == 0 and flag_allow_empty:
                flag_ok = True
                break

            try:
                option = int(s_option)
                if 0 <= option <= no_options:
                    flag_ok = True
                    break
            except ValueError:
                print "Invalid integer value!"

            print "Invalid option, range is [%d, %d]!" % (0, no_options)

            n_try += 1
            s_option = raw_input("? ")

        if flag_ok:
            break
    return option