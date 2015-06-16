
__all__ = ["path_to_default", "new_filename"]

import os.path

def path_to_default(fn):
    """Returns full path to default data file."""
    p = os.path.join(os.path.dirname(os.path.realpath(__file__)), "default", fn)
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