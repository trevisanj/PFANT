Introduction
============

PFANT is a stellar spectral synthesis software written in Fortran.

The development started with M Spite *et al.* in France in 1967 (Figure
0). The code formulation is described in `(Barbuy 1982) <#R_BLB1982>`__,
`(Cayrel et al. 1991) <#R_CAYREL1991>`__, `(Barbuy et al.
2003) <#R_BLB2003>`__, and `(Coelho et al. 2005) <#R_PC2005>`__.

::

     |
     | 1967 -- FANTÔME   -- by M Spite et al.
     | 1982 -- FANTOMOL  -- B Barbuy included the computation of molecular lines,
     |                      dissociatipon equilibrium
     | 2003 -- PFANT     -- M-N Perrin: large wavelength coverage,
     |                      inclusion or hydrogen lines
     | 2015 -- PFANT     -- J Trevisan: source code conversion to Fortran 2003 and optimization;
     |                      documentation; incorporation of MARCS opacities; Python interface, tools and API
    t|
     V

Figure 0 - Summarized PFANT timeline `(Coelho et al. 2005) <#RPC_2005>`__

Acknowledgement
---------------

The source code conversion, the documentation, incorporation of MARCS opacities,
and Python development was
funded by FAPESP - Research Support Foundation of the State of São Paulo, Brazil (2015-2017).

Contact
-------

For bugs reports, questions, suggestions, etc., please open an issue at the project
site on GitHub: `<http://github.com/trevisanj/PFANT>`_.


References
----------

**(Barbuy 1982)**\  Barbuy, B. 1982, PhD Thesis, Université de Paris VII

**(Cayrel et al. 1991)**\  Cayrel, R., Perrin, M. N., Barbuy, B., &
Buser, R. (1991). A grid of synthetic spectra for the determination of
effective temperature, gravity and metallicity of F, G, and K stars.
I-Description of the method. II-Application to 41 stellar spectra taken
in the Basel field of SA 141. Astronomy and Astrophysics, 247, 108-129.

**(Barbuy et al. 2003)**\  Barbuy, B., Perrin, M.-N., Katz, D. et al.
2003, A&A, 404, 661

**(Coelho et al. 2005)**\  Coelho, P., Barbuy, B., Meléndez, J., Schiavon, R. P., & Castilho, B. V.
(2005). A library of high resolution synthetic stellar spectra from 300 nm to
1.8 μm with solar and α-enhanced composition. Astronomy & Astrophysics, 443(2), 735-746.
