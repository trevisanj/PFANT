```
This directory

fortran
├── *.f90  ........... Source code
├── make-linux.sh  ... Compilation shell script for Linux
└── *.cbp , PFANT*  .. Code Blocks Fortran files

```

# Fortran directory

# Table of Contents

  1. [Compile](#S1)
  2. [Coding style](#S2)
  3. [Coding how-to's](#S3)
  4. [Appendix -- CodeBlocks Fortran IDE](#A)

# <a name=S1></a> 1 Compile

## 1.1 Using shell script

The quickest way to compile (in Linux) is to use the shell script provided:

```shell
./make-linux.sh
```

## 1.2 Using gfortran explicitly

The four programs `innewmarcs`, `hydro2`, `pfant`, `nulbad` have their corresponding ".f90" source files, _i.e._, _innewmarcs.f90_ etc. They must be linked together with  _pfantlib.f90_. The following will generate the four executable binaries.

```shell
gfortran -c pfantlib.f90
gfortran -o innewmarcs pfant.f90 pfantlib.o
gfortran -o hydro2 hydro2.f90 pfantlib.o
gfortran -o pfant pfant.f90 pfantlib.o
gfortran -o nulbad nulbad.f90 pfantlib.o
```

## 1.3 Using CodeBlocks Fortran

CodeBlocks Fortran (CBFortran) is an option for those who like to work with integrated development environments. CBFortran was used to create the make files in this directory. For more information, please refer to the [Appendix](#S4).

# <a name="S2"></a> 2 Coding style

This sections contains several guidelines to keep the source code consistent and
well documented.

## 2.1 Fortran file format

Now using the ".f90" (1990+) file format (instead of ".f").

## 2.2 Indentation

- 2-space indentation for all `if`, `do`, `subroutine` etc.

- Continuation lines: suggestion: continue with a 1-space indentation from the
  beginning line, i.e.:

```
... WRITE (*,*) 'A equals ', A, '; B equals ', B, '; C equals ', &
...  C, 'D equals ', D
     |
     1-space indentation after beginning line
```

## 2.3 Number of columns

Coders ofter pronounce on keeping code width to 80 columns maximum (this applies to all text files,
including `*.f`, `*.f90`, `*.txt`, `*.md`).

Common arguments are for for readability, ergonomics, and ability to split screen.
(http://www.cesm.ucar.edu/working_groups/Software/dev_guide/dev_guide/node7.html,
http://programmers.stackexchange.com/questions/148677/why-is-80-characters-the-standard-limit-for-code-width,
https://google-styleguide.googlecode.com/svn/trunk/javaguide.html#s4.4-column-limit).

I tend to [ab]use a **90-column maximum**.

## 2.4 Do **not** use `common` blocks

`common` blocks require variables to be declared multiple
times with different names and shapes. This is high-maintenance and makes the code harder
to understand, specially for those who are not familiar with the code.

Using `module` is a better way to share variables among subroutines and functions.
Variables are declared only once at the header section of a module.

## 2.5 Always `implicit none`

Add the `implicit none` statement at the beginning of each `module` or `program`.
  - rules out the risk of accidentally using undeclared symbols in formulae
  - types of variables becomes clear from reading the code
  - we are forced to remember to declare real variables as `real*8`

## 2.6 Variable declarations

### 2.6.1 `subroutine`/`function` arguments

  - always include `intent(in)`, `intent(out)`, or `intent(inout)` in subroutine/function argument declarations. 
    This has two advantages:
    - it counts as documentation
    - helps with bug prevention (the code will not compile if you try to write to a variable
      that has been declared using `intent(in)` 
  - declare only one argument per code line and write a short description as a comment

### 2.6.2 `module` variables

  - declare only one variable per code line and write a short description as a comment

### 2.6.3 Prefixes

In many sections of the code, a preceding `<prefix>_` has been added to
the original names of variables that are shared among subroutines and functions. 
Prefixes help to track the meaning and origin of a certain variable.
Additionally, they help to ensure that variable names don't clash across different modules.

Prefix examples:
```
au_g3d           from module absoru
config_fn_main   from module config
main_ptdisk      from module reader_main
selekfh_fl       calculated by subroutine synthesis::selekfh()
MAX_PARTIT_NPAR  constant having maximum allowed value of variable partit_npar
```

## 2.7 Real numbers

`real*8` (double precision) is now used throughout, except for reading the binary "*.mod" and "*.moo" files (which have 
floating-point numbers stored as `real*4`, so there is no getting away with this).

Here is a quote:

> If you must use floating point, use double precision unless you have reason
> to be concerned about memory use (your program uses large arrays) and you do
> not need much precision. Modern computers do not take significantly longer to
> process double precision values than they do to process reals.
(http://www.cs.uwm.edu/~cs151/Bacon/Lecture/HTML/ch06s09.html)

## 2.8 Commenting the code

It is recommended to document at least this:

- Explain -- at the beginning of a module -- what the prefixes of variables declared within it stand for
- `module`, `subroutine`, and `function`: at least one description line
- `subroutine` or `function` arguments: at least one sentence for each argument
- variables declared in the header section of a module: at least one sentence
- when the logic becomes tricky, it is a kind gesture (both for others and for yourself in the future) to explain what the code is doing

For more on software documentation, there is an inspiring treaty available at 
http://www.agilemodeling.com/essays/agileDocumentation.htm.

## 2.9 Writing messages to console

For outputting messages to the console, it is preferrable to use the routines from the `logging`
module instead of `write` or `print`. This may give a bit more work
(may require one extra line of code if the string is formatted), but
by using this, the user of the program has options to silence these messages
depending on their severity, and/or redirect logging message to a file.

There are different routines that can be used for this: 
`log_critical()`, `log_error()`, `log_warning()`, `log_info()`, `log_debug()`, `log_halt()` and also `log_and_halt()`.

## 2.10 Tags

There are some search keywords ("tags") created by convention that help keep track of issues and to-do's in the code:

```
todo      ordinary to-do item.
issue     high-priority to-do item, usually an unsolved issue about the way the code works.
?doc?     documentation "gap"
?what?    high-priority documentation gap, e.g. subroutine lacks description line.
ask BLB   suggestion on who to ask about issue
ask MT    "
ask PC    "
ask EC    "
```
One of this project goals is to get rid of all these tags (by resolving them).

Tags are case-insensitive.
 
# <a name="S3"></a> 3 Development how-to's
 
This section [non-exhaustively] describes how to carry out specific tasks with the source code, such as implementing new features.

## 3.1 How to create a new command-line option

To add a new command-line option to the Fortran code:

.1. Open _pfantlib.f90_

.2. Find string `module config`

.3. Create a new variable starting with `config_` and set its default value

.4. In subroutine `init_options()`, add a `call add_option(...)`

.5. In subroutine `handle_option`, add a `case` for your new variable. :bulb:

.6. Finally, use your new `config_xxxx` variable in other modules of the source code,
   making sure that the module that you are working on has a `use config` statement 
   at its beginning.
         
**Python code**

Steps 7-12 explain how to perform changes to the _pyfant_ package. The objectives are:
  - make new option accepted by `run4.py`
  - make the new option appear in the `x.py` graphical interface

.7. Clone the repository http://github.com/trevisanj/astrogear

.8. Find and open file _fileoptions.py_

.9. Add `self.xxxx = None` inside `FileOptions.__init__()` where `xxxx` is the name of your new
   option, without the config_ prefix

Steps 9-10 add the new option to Tab 3 of `x.py`

.10. Find and open file _a_WOptionsEditor.py_

.11. You will need to make a few interventions inside the `__init__()` method,
which should become clear from the existing code

.12. Push your changes to GitHub and create a new pull request

# <a name="A"></a>Appendix -- CodeBlocks Fortran IDE (CBFortran)
 
## A.1 Introduction

CBFortran is a customization of the Code Blocks (CB) IDE optimized for working with Fortran projects.

CBFortran does not require installation. Just download the .taz.bz2 file from the
downloads page http://cbfortran.sourceforge.net/downloads.html and extract it into your
home directory (will create directory _/home/your-user-name/CodeBLocks_Fortran_xxxx_).
 
After extracted, enter the new directory, and
execute `codeblocks_run.sh`

### A.2 Using CBFortran

The CBFortran project is the file _PFANT-linux.cbp_ or _PFANT-windows.cbp_
 
**Some shortcuts:**

- Find in files: `Ctrl+Shift+F`
- Compile: `Ctrl+F9`
- Compile everything from scratch: `Ctrl+F11` (required sometimes as incremental compilation is not perfect)
- Run: `Ctrl+F10`

**How make files were generated**

The following procedure is required for the script `make-linux.sh` to continue working if ".f90" files are added or removed to the project.

- Click on menu: Build &rarr; Select Target &rarr; innewmarcs/hydro2/pfant/hulbad
- Click on menu: Fortran &rarr; Generate Makefile
- Rename file _Makefile_, _e.g_, to _makefile-linux-innewmarcs_
- Repeat the previous steps for all four "targets".

