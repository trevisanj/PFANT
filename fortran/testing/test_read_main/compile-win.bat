@rem Generates pfantgrade.exe
mingw32-gfortran -c -g ..\main\commons.f
mingw32-gfortran -c -g ..\main\main_io.f
mingw32-gfortran -c -g test_read_main.f
mingw32-gfortran -o test_read_main test_read_main.o main_io.o commons.o
