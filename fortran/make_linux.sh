#!/bin/bash

# To make sure no old executables/object files remain, we explicitly delete them.
rm bin/*
rm -rf obj-linux/*


make -f makefile_linux_innewmarcs
make -f makefile_linux_hydro2
make -f makefile_linux_pfant
make -f makefile_linux_nulbad

