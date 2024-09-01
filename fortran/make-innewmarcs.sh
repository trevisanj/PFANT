#!/bin/bash

# To make sure no old executables/object files remain, we explicitly delete them.
rm bin/innewmarcs
rm -rf obj-linux/*

make -f makefile-linux-innewmarcs

