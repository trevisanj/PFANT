@rem Generates pfantgrade.exe
mingw32-gfortran -c -g pfantgrade.f
mingw32-gfortran -c -g pkapgeralgrade.f
mingw32-gfortran -o pfantgrade pfantgrade.o pkapgeralgrade.o


@rem Generates nulbadgrade.exe
mingw32-gfortran -c nulbadgrade.f
mingw32-gfortran -o nulbadgrade nulbadgrade.o
