# AGENTS.md

## Scope

This directory contains the Fortran implementation of PFANT, a stellar
spectral-synthesis package. These instructions apply to all files below
`fortran/`. The repository-level README is `../README.md`; developer notes are
in `README.md`, and the user manual source is under `../docs/source/`.

## Program pipeline

The normal calculation pipeline is:

1. `innewmarcs`: interpolate an atmospheric model from a MARCS grid. It
   normally creates `modeles.mod` and optionally `modeles.opa`/`modeles.asc`.
2. `hydro2`: calculate hydrogen-line profiles such as `thalpha` and `thbeta`.
3. `pfant`: synthesize the spectrum, continuum, and normalized spectrum as
   `flux.spec`, `flux.cont`, and `flux.norm` by default.
4. `nulbad`: convolve and resample a synthesized spectrum.

The external Python tooling documented in the manual (`run4.py`, `link.py`,
editors, and plotting commands) is not implemented in this directory.

## Source map

- `pfantlib.f90`: shared modules used by every executable. It contains array
  dimensions, logging, utilities, command-line configuration, file readers and
  writers, atmospheric-model types, opacity calculations, turbulence,
  molecular calculations, and the umbrella `pfantlib` module.
- `innewmarcs.f90`: atmospheric-grid interpolation executable.
- `hydro2.f90`: hydrogen-line physics and executable.
- `pfant.f90`: main spectrum-synthesis implementation and executable.
- `nulbad.f90`: convolution/resampling implementation and executable.
- `_convmol.f90`: molecular line-list conversion program. Its build is
  currently suspended and the generated makefile expects `convmol.f90`.
- `paula2003.f90`: historical/legacy source; it is not part of the normal
  Linux build.
- `makefile-linux-*`: generated makefiles for individual executables.
- `make-linux.sh`: builds the four normal executables into `bin/`, placing
  objects and module files in `obj-linux/`.

`pfantlib.f90` is intentionally compiled before each program. Changes to one of
its modules can affect all four executables.

## Build and verification

Prerequisite: `gfortran` and `make`.

Build all supported executables:

```sh
./make-linux.sh
```

Build one executable while iterating:

```sh
make -f makefile-linux-pfant
make -f makefile-linux-innewmarcs
make -f makefile-linux-hydro2
make -f makefile-linux-nulbad
```

The generated binaries are `bin/innewmarcs`, `bin/hydro2`, `bin/pfant`, and
`bin/nulbad`. Run `bin/<program> --help` to inspect its supported options.

There is currently no automated test suite in this directory. At minimum,
verify a code change by rebuilding every affected executable. For numerical or
file-format changes, also run the appropriate stage against a copied fixture
assembled from `../data/common/` and one star directory under `../data/`. Do
not overwrite committed input fixtures or assume that generated spectra belong
in the repository.

## Runtime data

Star-dependent examples live in directories such as `../data/arcturus/`,
`../data/mu-leo/`, and `../data/sun-asplund-2009/`. They commonly provide:

- `main.dat`: stellar parameters and calculation settings.
- `abonds.dat`: chemical abundances.
- `dissoc.dat`: molecular dissociation data.

Shared data in `../data/common/` includes atmospheric grids, atomic and
molecular line lists, continuous-opacity data, partition functions, and the
hydrogen-line map. Programs find these through conventional filenames or
`--fn_*` command-line overrides defined in `module config` in `pfantlib.f90`.

Several model formats are binary and depend on explicit record layouts and
`real*4` compatibility. Treat changes to declarations, record lengths, array
dimensions, or read/write ordering as file-format changes and test round trips
carefully.

## Coding conventions

Follow the conventions documented in `README.md`:

- Use free-form `.f90` source, two-space indentation, and approximately a
  90-column maximum.
- Add `implicit none` to every program and module.
- Prefer modules over `common` blocks.
- Use `real*8` unless compatibility with an existing binary format requires
  `real*4`.
- Give procedure arguments explicit `intent(in)`, `intent(out)`, or
  `intent(inout)` declarations.
- Use the routines in `module logging` instead of direct console `write` or
  `print` calls.
- Preserve established module-variable prefixes such as `config_`, `main_`,
  and module-specific prefixes.
- Keep comments focused on physical meaning, units, file-format constraints,
  and non-obvious numerical behavior.

When adding a command-line option, update both `init_options()` and
`handle_option()` in `module config`, then use the corresponding `config_*`
variable from the consuming module. Option applicability is encoded by the
program letters passed to `add_option` (`i`, `h`, `p`, `n`, and `c`).

## Change discipline

- Preserve unrelated working-tree changes and generated artifacts already
  present in `bin/` and `obj-linux/`.
- Avoid broad mechanical rewrites of `pfantlib.f90`; review changes by module.
- Do not silently alter default filenames or binary layouts, because external
  PFANT/PyFANT workflows depend on them.
- Keep the original scientific formulas intact unless the task explicitly
  calls for a numerical change. Document the source and units of any revised
  constants.
- When diagnosing numerical differences, compare intermediate stage outputs
  before changing downstream synthesis or convolution code.

## Known repository quirks

- The explicit compilation example in `README.md` names `pfant.f90` for the
  `innewmarcs` command; the actual source is `innewmarcs.f90`.
- `makefile-linux-convmol` expects `convmol.f90`, while the checked-in source is
  `_convmol.f90`, and its invocation is commented out in `make-linux.sh`.
- The makefiles are generated and repeat compilation of `pfantlib.f90`; avoid
  assuming a sophisticated dependency graph or incremental multi-target build.
