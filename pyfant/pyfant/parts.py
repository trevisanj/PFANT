"""
This module contains "parts" classes -- classes be used in multiple inheritance
"""

__all__ = ["AttrsPart", "PyfantObject", "froze_it"]

from functools import wraps

def froze_it(cls):
    """
    Decorator to prevent from creating attributes in the object ouside __init__().

    Yoann's answer at http://stackoverflow.com/questions/3603502
    """
    cls._frozen = False

    def frozensetattr(self, key, value):
        if self._frozen and not hasattr(self, key):
            raise AttributeError("Class {} is frozen. Cannot set {} = {}"
                  .format(cls.__name__, key, value))
        else:
            object.__setattr__(self, key, value)

    def init_decorator(func):
        @wraps(func)
        def wrapper(self, *args, **kwargs):
            func(self, *args, **kwargs)
            self._frozen = True
        return wrapper

    cls.__setattr__ = frozensetattr
    cls.__init__ = init_decorator(cls.__init__)

    return cls

# Didn't work, trying to freeze the final classes
#@froze_it
class PyfantObject(object):
    """Descend from this class to prevent new attributes being created when someone
    tries to set attribute that does not exist."""

class AttrsPart(object):
    """
    Implements a new __str__() to print selected attributes.

    Note: when descending from this class, set the attrs class variable.
    """

    # for __str__()
    attrs = None

    def __str__(self):
        assert self.attrs is not None, "Forgot to set attrs class variable"

        s_format = "{:>%d} = {}" % max([len(x) for x in self.attrs])
        s = '' # object.__str__(self)+"\n"
        s = "\n".join([s_format.format(x, self.__getattribute__(x)) for x in self.attrs])
        return s
