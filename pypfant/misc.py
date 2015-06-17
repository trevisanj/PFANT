
__all__ = ["str_vector", "float_vector", "path_to_default", "new_filename", "str2bool",
           "write_lf", "bool2str", "list2str"]

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

