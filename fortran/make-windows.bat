@rem To make sure no old executables remain, we explicitly delete them.
del bin\*.*
del obj-windows*.* /s

make -f makefile-windows-innewmarcs
make -f makefile-windows-hydro2
make -f makefile-windows-pfant
make -f makefile-windows-nulbad

