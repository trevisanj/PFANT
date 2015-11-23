@rem To make sure no old executables remain, we explicitly delete them.
del bin\*.*

make -f makefile_windows_innewmarcs
make -f makefile_windows_hydro2
make -f makefile_windows_pfant
make -f makefile_windows_nulbad

