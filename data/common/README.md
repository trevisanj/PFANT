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
gridsmap.dat   | list of MARCS atmospheric model grid files
newnew*.mod    | MARCS atmospheric model grid files


## Suggestion on how to use these files

Instead of making copies of these files, create symbolic links to them by using the following shell command:

```shell
link-to-data.py common
```

## References

[1] Coelho, P., Barbuy, B., Meléndez, J., Schiavon, R. P., & Castilho, B. V. (2005). 
A library of high resolution synthetic stellar spectra from 300 nm to 1.8${\ rm\ mu} $ m 
with solar and $\ alpha $-enhanced composition. Astronomy & Astrophysics, 443(2), 735-746.
