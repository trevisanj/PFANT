
from distutils.core import setup
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
    install_requires = ['pyqt4', 'matplotlib', 'fortranformat'],
    scripts = glob('scripts/*.py')  # Considers system scripts all .py files in 'scripts' directory

)