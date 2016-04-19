# Common data files

Data files that are independent from the star.

filename       | description
---------------|-------------
atoms.dat      | atomic lines (todo reference) (from 3688.331 to 8194.824 angstrom) (original name was `atomgrade.dat`)
atom4070g.dat  | atomic lines (todo reference) (from 3688.331 to 7711.724 angstrom) 
molecules.dat  | molecular lines (21 molecules) [1]
mghc2cnr_cn3ch13chtiog3tio.dat | molecular lines (12 molecules) (before 2005)
absoru2.dat    | absoru data
hmap.dat       | hydrogen lines information
partit.dat     | partition functions 
grid.mod       | grid of MARCS atmospheric models (*)
grid.moo       | grid of MARCS atmospheric models with opacities included (*)


(*) grid specifications:
  - Chemical composition class: Moderately CN-cycled
  - Model geometry: Spherical
  - Microturbulence parameter: 2
  - Mass: 1
  - Teff: 3500 .. 7000
  - Glog: 0 .. 3
  - [Fe/H]: -2 .. 0.25

**Note** If you want to use MARCS opacities, please download file `grid.moo` from [this location]
(https://drive.google.com/file/d/0B8m8GNLFiaewY0J1YzRrbHBCbWs/view?usp=sharing)].
This file has more than 100 MB and therefore cannot be stored in github.

**Create symbolic links to these files** 

instead of making copies of them:

```shell
link.py common
```

## References

[1] Coelho, P., Barbuy, B., Meléndez, J., Schiavon, R. P., & Castilho, B. V. (2005). 
A library of high resolution synthetic stellar spectra from 300 nm to 1.8${\ rm\ mu} $ m 
with solar and $\ alpha $-enhanced composition. Astronomy & Astrophysics, 443(2), 735-746.
