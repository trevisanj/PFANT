Conversion of molecular lines lists
===================================

Introduction
------------

This section describes the algorithm for conversion of molecular lines lists.

Source formats.

- Robert Kurucz molecular line lists [Kurucz]_ (functional)
- HITRAN Online database [Gordon2016]_ (partially implemented)
- VALD3 [VALD3]_ (to do)
- TurboSpectrum [Plez]_ (to do)

Conversion output:

- PFANT molecular lines file (such as "molecules.dat")

.. note:: This is work in progress and the conversion is still in experimental development stage.

How to convert molecular lines
------------------------------

This section is a short tutorial on converting molecular linelists.

#. Create a "project" directory
#. Run ``moldbed.py`` (:numref:`moldbed0`) and press "Ctrl+D" to
   spawn a new *molecular constants database* in your local directory.
   The name of such file defaults to "moldb.sqlite".
   This operation only needs to be carried out once for each "project" directory
#. Download linelist file, e.g., from [Kurucz]_
#. Run ``convmol.py``
#. In the first tab (:numref:`convmol0`)

   * Press "Ctrl+D" (only if the form is disabled)
   * Fill in the form as desired
   * Press "Ctrl+S" to save configuration file for this tab

#. In the second tab (:numref:`convmol1`):

   * press "Ctrl+D" (only if the form is disabled)
   * select "Kurucz" as data source on the left
   * locate linelist file
   * select isotope
   * most probably, check flags as in (:numref:`convmol1`)
   * specify output filename, or click on the plant button to make it up
   * Press "Ctrl+S" to save configuration file for this tab
   * **Click on "Run conversion" button**. Wait for conversion to complete

#. In the third tab (:numref:`convmol2`), see details about the conversion session.

.. _moldbed0:

.. figure:: img/moldbed0.png
    :align: center

    -- First tab of ``convmol.py``

.. _convmol0:

.. figure:: img/convmol0.png
    :align: center

    -- First tab of ``convmol.py``

.. _convmol1:

.. figure:: img/convmol1.png
    :align: center

    -- Second tab of ``convmol.py``

.. _convmol1:

.. figure:: img/convmol1.png
    :align: center

    -- Third tab of ``convmol.py``


How the conversion is made
--------------------------

This section describes the conversion algorithm itself.

List of symbols
~~~~~~~~~~~~~~~

Input molecular constants obtained from NIST database (all given in unit: cm**-1)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* *omega_e*: vibrational constant – first term
* *omega_ex_e*: vibrational constant – second term
* *omega_ey_e*: vibrational constant – third term
* *B_e*: rotational constant in equilibrium position
* *alpha_e*: rotational constant – first term
* *D_e*: centrifugal distortion constant
* *beta_e*: rotational constant – first term, centrifugal force
* *A*: Coupling counstant
* *M2l*: multiplicity of the initial state (1 for singlet, 2 for doublet, 3 for triplet and so on)
* *M2l*: multiplicity of the final state
* *LambdaL*: Sigma/Pi/Delta/Phi of the initial state (0 for Sigma, 1 for Pi, 2 for Delta, 3 for Phi)
* *Lambda2L*: Sigma/Pi/Delta/Phi of the initial state

Input data from line list files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* *lambda*: wavelength (angstrom)
* *vl*: vibrational quantum number of the initial state
* *v2l*: vibrational quantum number of the final state
* *spinl*
* *spin2l*
* *JL*: rotational quantum number of the initial state
* *J2l*: rotational quantum number of the final state

Calculated outputs
^^^^^^^^^^^^^^^^^^

The following values are calculated using application ``convmol.py`` and stored as a PFANT molecular lines file (such as "molecules.dat").

*Jl*/*J2l*-independent
++++++++++++++++++++++

* *qv*: Franck-Condon factor
* *Bv*: rotational constant
* *Dv*: rotational constant
* *Gv*: rotational constant

These terms are calculated as follows::

    qv = qv(molecule, system, vl, v2l) is calculated using the TRAPRB code [TRAPRB1970]
                                       The Franck-Condon factors were already calculate for several
                                       different molecules and are tabulated inside file "moldb.sqlite"

    Bv = B_e - alpha_e * (v2l + 0.5)

    Dv = (D_e + beta_e * (v2l + 0.5)) * 1.0e+06

    Gv = omega_e * (v2l + 0.5) - omega_ex_e * (v2l + 0.5) ** 2 + omega_ey_e * (v2l + 0.5) ** 3 -
         omega_e / 2.0 - omega_ex_e / 4.0 + omega_ey_e / 8.0


*Jl*/*J2l*-dependent (*i.e.*, for each spectral line)
+++++++++++++++++++++++++++++++++++++++++++++++++++++

* *LS*: line strength for given by formulas in [Kovacs1969]_, Chapter 3; Hönl-London factor
* *S*: normalized line strength

*LS* is calculated using a different formula depending on:

i. the multiplicities of the transition (currently implemented only cases where the initial and
   final state have same multiplicity)
ii. the value and/or sign of (*DeltaLambda* = *LambdaL* - *Lambda2l*);
iii. whether *A* is a positive or negative number;
iv. the branch of the spectral line (see below how to determine the branch)

So::

    formula = KovacsFormula(i, ii, iii, iv)

    LS = formula(almost every input variable)

.. todo::

    Explain term formulas "u+/-", "c+/-"

Normalization of the line strength
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Normalization is applied so that, for a given *J2l*,::

    sum([S[branch] for branch in all_branches]) == 1

To achieve this::

    S = LS * 2. / ((2 * spin2l + 1) * (2 * J2l + 1) * (2 - delta_k))

Where::

    spin2l = (M2l-1)/2

How to determine the branch
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The branch "label" follows one of the following conventions::

    singlets: branch consists of a "<letter>", where letter may be either "P", "Q", or "R"

    doublets, triplets etc:

        if spin == spinl == spin2l: branch consists of "<letter><spin>"

        if spinl <> spin2l: branch consists of "<letter><spinl><spin2l>"


The branch letter is determined as follows::

    if Jl < J2l:  "P"
    if Jl == J2l: "Q"
    if Jl > J2l:  "R"

Where the conversion code is located
------------------------------------

- The line strength formulas from [Kovacs1969]_ are in module ``pyfant.kovacs``
  (source code directly available for inspection at
  `<https://github.com/trevisanj/pyfant/blob/master/pyfant/kovacs.py>`_)
- The conversion routines are in subpackage ``pyfant.convmol`` (source code at
  `<https://github.com/trevisanj/pyfant/tree/master/pyfant/convmol>`_)

Bibliography
------------
.. [Kovacs1969] Istvan Kovacs, Rotational Structure in the spectra of diatomic molecules. American Elsevier, 1969

.. [TRAPRB1970] Jarmain, W. R., and J. C. McCallum. "TRAPRB: a computer program for molecular
   transitions." University of Western Ontario (1970)

.. [NIST] http://webbook.nist.gov/chemistry/

.. [Kurucz] http://kurucz.harvard.edu/molecules.html

.. [VALD3] http://vald.astro.univie.ac.at/~vald3/php/vald.php

.. [Plez] http://www.pages-perso-bertrand-plez.univ-montp2.fr/

.. [Gordon2016] I.E. Gordon, L.S. Rothman, C. Hill, R.V. Kochanov, Y. Tan, P.F. Bernath, M. Birk,
   V. Boudon, A. Campargue, K.V. Chance, B.J. Drouin, J.-M. Flaud, R.R. Gamache, J.T. Hodges,
   D. Jacquemart, V.I. Perevalov, A. Perrin, K.P. Shine, M.-A.H. Smith, J. Tennyson, G.C. Toon,
   H. Tran, V.G. Tyuterev, A. Barbe, A.G. Császár, V.M. Devi, T. Furtenbacher, J.J. Harrison,
   J.-M. Hartmann, A. Jolly, T.J. Johnson, T. Karman, I. Kleiner, A.A. Kyuberis, J. Loos,
   O.M. Lyulin, S.T. Massie, S.N. Mikhailenko, N. Moazzen-Ahmadi, H.S.P. Müller, O.V. Naumenko,
   A.V. Nikitin, O.L. Polyansky, M. Rey, M. Rotger, S.W. Sharpe, K. Sung, E. Starikova,
   S.A. Tashkun, J. Vander Auwera, G. Wagner, J. Wilzewski, P. Wcisło, S. Yu, E.J. Zak,
   The HITRAN2016 Molecular Spectroscopic Database, J. Quant. Spectrosc. Radiat. Transf. (2017).
   doi:10.1016/j.jqsrt.2017.06.038.
