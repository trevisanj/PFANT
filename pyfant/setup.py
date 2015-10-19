"""
Setup script for the future


from setuptools import setup
from glob import glob
setup(
    name = 'PyFANT',
    packages = ['pyfant'],
    version = '0.1',
    description = 'Python layer and utilities for the PFANT spectral synthesis software',
    author = 'Julio Trevisan',
    author_email = 'juliotrevisan@gmail.com',
    url = 'https://github.com/trevisanj/pfant', # use the URL to the github repo
    download_url = 'https://github.com/trevisanj/pfant/tarball/0.1', # I'll explain this in a second
    keywords = ['spectral synthesis'],
    classifiers = [],
    install_requires = ['matplotlib', 'fortranformat'],  # matplotlib never gets installed correctly by pip, byt anyway...
    scripts = glob('scripts/*.py')  # Considers system scripts all .py files in 'scripts' directory

)
"""

import os.path

my_dir = os.path.dirname(os.path.realpath(__name__))
SEP = "*"
print ""
print SEP*79
print "Installing through Python setup engine is not available at the moment."
print ""
print "Because the package is under construction, it is currently best to add"
print "the directory"
print ""
print my_dir
print ""
print "to your PYTHONPATH environment variable (PFANT/add_paths.py may help),"
print "and \"git pull\" often."
print SEP*79
print ""
