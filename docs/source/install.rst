Installation
============

To use PFANT, you will need to:

#. Download files
#. Compile the Fortran source code
#. Add ``PFANT/fortran/bin`` to your PATH
#. Install the "f311" Python package (http://github.com/trevisanj/f311)
   (recommended)

This section will take you through these steps.

.. note:: PFANT is platform-independent (it should work no any system if you can install the GNU
          Fortran Compiler), however only Debian-based Linux system is "supported" in the following
          instructions. Windows users will find some tips in a specific section below.

Installing required software
----------------------------

Standalone applications
~~~~~~~~~~~~~~~~~~~~~~~

Please install the following standalone applications on your system (no
pain except for gfortran and make on Windows (see below)):

-  gfortran (version 4.8 recommended; does **not** compile with version
   4.4)
-  make

Download files
--------------

Clone the GitHub repository:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: shell

    git clone https://github.com/trevisanj/PFANT

This will create a directory named PFANT on your disk.

Compiling the Fortran source code.
----------------------------------

Enter the following on your console to compile the Fortran source code:

.. code:: shell

    cd PFANT
    cd fortran
    ./make-linux.sh

This should create four executable binaries inside the directory *PFANT/fortran/bin*:
``innewmarcs``, ``hydro2``, ``pfant``, ``nulbad``.

Setting the paths
-----------------

Depending on which shell your system uses, try one of the following:

Bash shell:

.. code:: shell

    ./add-path.py --bash

Tcsh shell:

.. code:: shell

    ./add-path.py --tcsh

This will automatically apply the path settings to your *home/.bashrc*
or *home/.cshrc*.

.. note:: If the above does not work for you, manually add
          *PFANT/fortran/bin* to your system path.

Install ``pyfant`` Python package
-----------------------------

Although PFANT contains standalone a set of tools for spectral synthesis, it is recommended
to install the ``pyfant`` Python package to add running, editing, visualization and conversion capabilities
around the Fortran core. Installation instructions are available at
http://trevisanj.github.io/pyfant

Tips for windows users
----------------------

gfortran and make on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MinGW (http://sourceforge.net/projects/mingw/files/) is a convenient way
to install the GNU Fortran compiler on Windows.

After installed, MinGW has its own package manager, named
"MinGW Installation Manager". There, you will need to install at least
the following packages: ``mingw-developer-toolkit``, ``mingw32-base``,
``mingw32-gcc-fortran``, ``msys-base``.

Compiling the source code on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The source can be compiled using the CodeBlock Fortran IDE.
The *PFANT/fortran* forder contains a CodeBlocks project named *PFANT-windows.cbp*.
