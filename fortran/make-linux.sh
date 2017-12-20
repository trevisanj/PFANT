#!/bin/bash

# To make sure no old executables/object files remain, we explicitly delete them.
rm bin/*
rm -rf obj-linux/*


make -f makefile-linux-innewmarcs
make -f makefile-linux-hydro2
make -f makefile-linux-pfant
make -f makefile-linux-nulbad
# suspended make -f makefile-linux-convmol

