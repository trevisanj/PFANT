# specification of differential abundances for each chemical
# - differential abundance: number to add to original value in abonds.dat
# - all list of abundances for each element must have the same length
ab = {"Ca": [-.3, 0, .3, .5],
      "Si": [-.3, 0, .3, .5]}

# Convolutions specification for fwhm parameter:
# [first value, last value, step]
conv = [0.08, 0.6,  0.04]