#!/bin/sh

##########################################################################################
# PARAMETERS
############

Teffs="5000 5250 5500 5750 6000 6250 6500"  # Effective temperatures
loggs="3.0 3.5 4.0 4.5 5.0"                 # Gravities
met="-0.50"                                 # Metalicity
alpha="+0.20"                               # alpha-Fe
folder="pMARCS_z${met}"                     # Folder containing files from MARCS website
vt="01"                                     # Microturbulence velocity

##########################################################################################

#------------------
# REMOVE FILES
#------------------
rm -f modelesz${met}_a${alpha}.dat
rm -f modelesz${met}_a${alpha}.mod


#------------------
# CREATE FILE.DAT
#------------------
for Teff in ${Teffs};
do 
#
    for logg in ${loggs};
    do

        cat ./${folder}/p${Teff}_g+${logg}_m0.0_t${vt}_st_z${met}_a${alpha}_c+0.00_n+0.00_o${alpha}_r+0.00_s+0.00.mod >> modelesz${met}_a${alpha}.dat

    done
done


#------------------
# CREATE FILE.MOD
#------------------
./transosmarcsok3 << DONE
'modelesz${met}_a${alpha}.dat'
'marcs2009z${met}_a${alpha}.mod'
T
1
DONE


