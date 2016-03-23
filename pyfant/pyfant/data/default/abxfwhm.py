# Specification of differential abundances for each chemical.
# Differential abundance is a number to add to original value in abonds.dat.
# All lists of abundances for each element must have the same length.
ab = {"Ca": [-.3, 0, .3, .5],
      "Si": [-.3, 0, .3, .5]}

# Name for each pfant run. They will be part of spectrum file names.
# This is optional. If not used, sequential numbers will be used instead.
# Example: pfant_names = ["A", "B", "C", "D"]
pfant_names = []

# Convolutions specification for fwhm parameter:
# [first value, last value, step]
conv = [0.08, 0.6,  0.04]
