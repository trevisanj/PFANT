#!/bin/sh
gfortran -c absor.f
gfortran -c calr2.f
gfortran -c calr3.f 
gfortran -c abon2.f
gfortran -o abon2 abon2.o absor.o calr2.o calr3.o
