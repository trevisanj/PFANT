from pyfant.from_vald import *
from pyfant import misc
import logging

misc.logging_level = logging.DEBUG

FILENAME = 'CesarHenriqueSiqueiraMelloJunior.009511'

with open(FILENAME, 'r') as file:
    file_atoms = vald3_to_atoms(file)


file_atoms.save_as("atoms-vald3-10k-17k.dat")