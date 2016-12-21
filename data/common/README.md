# Common data files

Data files that are independent from the star.

:zap: If _grid.moo_ is not present, please download it as indicated in [PFANT/README.md, Section 2.2](../../README.md#22-downloading-pfant).

filename       | description
---------------|-------------
atoms.dat      | atomic lines (from 3688.331 to 8194.824 angstrom) (original name was `atomgrade.dat`)
molecules.dat  | molecular lines (21 molecules) [1]. This file is currently identical to (2016-12-21)
               | https://github.com/trevisanj/PFANT-data/blob/master/molecular-lines/moleculagrade-paula.dat.newtitulo
               | **there are a few wrong wavelengths** (2016-12-21)
               | Note that 'molecules.dat' is the default name for PFANT molecular lines file, therefore this file is
               | likely to change contents over time, reusing the same name
absoru2.dat    | absoru data
hmap.dat       | hydrogen lines information
partit.dat     | partition functions 
grid.mod       | grid of MARCS atmospheric models (see grid specs below)
grid.moo       | grid of MARCS atmospheric models with opacities included (see grid specs below)

## Atmospheric model grid specifications

The files grid.mod and grid.moo were created using `create-grid.py` having as input a bulk of
".mod" and ".opa" (the latter for grid.moo only) files downloaded from the MARCS website
(http://marcs.astro.uu.se/). At this site, when you are prompted to fill in a download form, here
is how the choices were made:

  - Chemical composition class: Moderately CN-cycled
  - Model geometry: Spherical
  - Microturbulence parameter: 2
  - Mass: 1
  - Teff: 3500 .. 7000
  - Glog: 0 .. 3.5
  - [Fe/H]: -2 .. 0.25

## Using these files

We that you create symbolic links to these files instead of making copies of them
(unless you want to change some file). To do so:

```shell
cd mystar
link.py common
```

## References

[1] Coelho, P., Barbuy, B., Meléndez, J., Schiavon, R. P., & Castilho, B. V. (2005). 
A library of high resolution synthetic stellar spectra from 300 nm to 1.8${\ rm\ mu} $ m 
with solar and $\ alpha $-enhanced composition. Astronomy & Astrophysics, 443(2), 735-746.
