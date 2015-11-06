"""
This package contains plotting functions.

Decision was made now to work with classes. A class interface for generic
input->plot->figure behaviour can be implemented later using these functions.

This package is supposed to import * to keep "users" from ever bothering about
this sub-package structure, which is likely to remain disorganized.

"""
from .plotsp import *
from .vis import *
