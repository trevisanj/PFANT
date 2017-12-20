
``hydro2``
~~~~~~~~~~

.. code:: shell

    hydro2 \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --fn_modeles modeles.mod \
        --fn_absoru2 absoru2.dat \
        --fn_hmap hmap.dat \
        --interp 1 \
        --kik 0 \
        --ptdisk <main_llzero>  \
        --llfin <main_llfin>  \
        --amores T \
        --kq 1 \
        --zph 12.00

``innewmarcs``
~~~~~~~~~~~~~~

.. code:: shell

    innewmarcs \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --fn_modeles modeles.mod \
        --fn_modgrid grid.mod \
        --fn_moo grid.moo \
        --allow F \
        --fn_opa modeles.opa \
        --opa T

``pfant``
~~~~~~~~~

.. code:: shell

    pfant \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --fn_modeles modeles.mod \
        --fn_absoru2 absoru2.dat \
        --fn_hmap hmap.dat \
        --interp 1 \
        --kik 0 \
        --ptdisk <main_llzero>  \
        --llfin <main_llfin>  \
        --fn_opa modeles.opa \
        --fn_partit partit.dat \
        --fn_abonds abonds.dat \
        --fn_atoms atoms.dat \
        --no_molecules F \
        --no_atoms F \
        --no_h F \
        --pas <main_pas>  \
        --aint <main_aint> \
        --opa T \
        --abs F \
        --opa T \
        --opa T \
        --fn_dissoc dissoc.dat \
        --fn_molecules molecules.dat \
        --flprefix <main_flprefix> 

``nulbad``
~~~~~~~~~~

.. code:: shell

    nulbad \
        --logging_console T \
        --logging_dump F \
        --logging_fn_dump <executable name>_dump.log \
        --fn_main main.dat \
        --flprefix <main_flprefix>  \
        --fn_flux <main_flprefix>.norm  \
        --flam F \
        --fn_cv <flux file name>.nulbad.<fwhm> \
        --pat <main_pas>  \
        --convol T \
        --fwhm <main_fwhm> 