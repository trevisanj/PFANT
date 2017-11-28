#!/bin/bash

# The following command forces Sphinx to be executed using whichever Python is "which python",
# not the Python executable specified in the sha-band in "/usr/local/bin/sphinx-build"
# Solution found in http://stackoverflow.com/questions/4122040/how-to-make-sphinx-look-for-modules-in-virtualenv-while-building-html

make html SPHINXBUILD="python /usr/local/bin/sphinx-build"
