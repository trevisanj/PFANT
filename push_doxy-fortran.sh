#!/bin/bash

# This script pushed the doxy-fortran directory into the gh-pages branch at github
#
# Reference: https://gist.github.com/cobyism/4730490

git subtree push --prefix doxy-fortran origin gh-pages
