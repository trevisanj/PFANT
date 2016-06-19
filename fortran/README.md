# Fortran source code

## Directory contents

*.f90, *.f      -- Fortran sources
                                     
## Tools

This section contains guidelines for developers to maintain and share the source code. 

### Working with the github repository
 
To clone the github repository:
 
```shell
git clone https://github.com/trevisanj/PFANT
```

This will create a directory named PFANT, which is treated as our root directory in this
document.

#### Communicating with github <i>via</i> SSH

This will facilitate future <code>git push</code> invokes.
To keep git from asking for your github username and password every time you execute a
<code>git push</code>, you have to configure git to use the SSH protocol to communicate
with github.
 
The steps involved are roughly summarized below. For full details, please follow the
tutorials referenced below.
1. Generate a SSH key. I followed the tutorial at
   https://help.github.com/articles/generating-ssh-keys/,
   but the commands are roughly as follows:
   ```shell
   ls -al ~/.ssh  # Check existing SSH keys
   ssh-keygen -t rsa -C "your@email.com"
   ssh-add ~/.ssh/id_rsa
   <your_text_editor>  ~/.ssh/id_rsa.pub
   ```
2. Copy-paste the key to a box at a settings page at GitHub.
   After you do this, you can test if it works:
   ```shell
   ssh -T git@github.com
   ```
3. Change the remote URL that git uses. Tutorial at
   https://help.github.com/articles/changing-a-remote-s-url/
   ```shell
   git remote set-url origin git@github.com:trevisanj/pfant.git
   ```
  
### CodeBlocks Fortran (CBFortran)
 
CBFortran is a customization of the
Code Blocks (CB) IDE optimized for working with Fortran projects.

I am not suggesting CBFortran to edit code, but to compile the code, as
**this IDE does a good job in compiling the code** (and also generating makefiles). 
CB was used to create the makefiles in the `PFANT/fortran directory`.
 
#### Running CBFortran on Linux
 
CBFortran does not require installation. Just download the .taz.bz2 file from the
downloads page http://cbfortran.sourceforge.net/downloads.html and extract it into your
home directory (will create directory `/home/user/CodeBLocks_Fortran_xxxx`).
 
After extracted, enter the new directory, and
execute `codeblocks_run.sh`
 
#### Using CBFortran
 
The CBFortran project is the file PFANT/fortran/PFANT.cbp.
 
Some shortcuts:

- Find in files: `Ctrl+Shift+F`
- Compile: `Ctrl+F9`
- Compile everything from scratch: `Ctrl+F11` 
- Run: `Ctrl+F10`


## Coding style

This sections contains several guidelines to keep the source code consistent and
well documented.

@todo how-to "How to convert user into a code developer"

@todo how to create a new executable

@todo how to insert new command-line option

@todo reading either from main file or command-line option for all variables

### Format

Now using the "modern" .f90-.f95 file format (instead of .f "punchcard").

#### Indentation

- 2-space indentation for all IF, DO, SUBROUTINE etc. structures.

- Continuation lines: suggestion: continue with a 1-space indentation from the
  beginning line, i.e.:

```
... WRITE (*,*) 'A equals ', A, '; B equals ', B, '; C equals ', &
...  C, 'D equals ', D
     |
     1-space indentation after beginning line
```

#### Number of columns

Coders ofter pronounce on keeping code width to 80 columns maximum (this applies to all text files,
including `*.f`, `*.f90`, `*.txt`, `*.md`).

Common arguments are for for readability, ergonomics, and ability to split screen.
(http://www.cesm.ucar.edu/working_groups/Software/dev_guide/dev_guide/node7.html,
http://programmers.stackexchange.com/questions/148677/why-is-80-characters-the-standard-limit-for-code-width,
https://google-styleguide.googlecode.com/svn/trunk/javaguide.html#s4.4-column-limit).

I tend to [ab]use a **90-column maximum**.


### Code structure

#### Do *not* use COMMON blocks

COMMON blocks require variables to be declared multiple
times with different names and shapes. This is high-maintenance and makes the code harder
to understand, specially for newbies.

MODULE provides a more clear structure to share variables among subroutines and functions.
Variables are declared only once at the header section of a module.

#### Variable names

*Prefixes*: in many sections of the code, a preceding `<prefix>_` has been added to
the original names of variables that are shared among subroutines and functions.
 
- Prefixes help to track the meaning and origin of a certain variable.
- Additionally, they help to ensure that variable names don't clash across different modules.

Sometimes the prefix matches the module name where the variable is declared, sometimes not.
    
Examples:
```
au_g3d           from module absoru
config_fn_main   from module config
main_ptdisk      from module reader_main
selekfh_fl       calculated by subroutine synthesis::selekfh()
MAX_PARTIT_NPAR  constant having maximum allowed value of variable partit_npar
```

#### Real numbers

`real*8` is now used throughout, except for reading the binary .mod files (which have 
floating-point numbers stored as real*4, so there is no getting away with this).

> If you must use floating point, use double precision unless you have reason
> to be concerned about memory use (your program uses large arrays) and you do
> not need much precision. Modern computers do not take significantly longer to
> process double precision values than they do to process reals.
(http://www.cs.uwm.edu/~cs151/Bacon/Lecture/HTML/ch06s09.html)


#### Always IMPLICIT NONE

Add the IMPLICIT NONE statement at the beginning of each MODULE or PROGRAM.

- types of variables becomes clear from reading the code
- we are forced to remember to declare real variables as `real*8`

#### Variable declarations

- **`subroutine`/`function` arguments**: always explicit whether the variable is an input
  or an output by using `intent(in)` or `intent(out)`. This has two advantages:
  - it is effective documentation
  - helps with bug prevention (the code will not compile if you try to write to a variable
    that has been declared using `intent(in)` 
- **Module variables and `subroutine`/`function` arguments**: declare *only one* variable per
    code line, and **document the variable**.
- **Any variable**: document the variable if its use/purpose is not obviuos.

#### `function`/`subroutine`

Write documentation explaining what the function/subroutine does and whenever possible,
why it was created.

#### Logging

Logging is about generating output about program activity, *e.g.*, printing
messages on the screen.

For outputting messages on the screen, you may use the routines from the `logging`
module instead of `write` or `print`. This may give a bit more work
(may require one extra line of code if the string is formatted), but
by using this, the user of the program has options to silence these messages
depending on their severity, and/or redirect logging message to a file.

There are different routines that can be used, such as
`log_critical()`, `log_error()`, `log_warning()`, `log_info()`, `log_debug()`.


#### Assertions

An assertion is a statement that an expression is expected to always be true 
at a given point in the code. If an assertion evaluates to `.false.` at run time,
the program will crash (https://en.wikipedia.org/wiki/Assertion_(software_development)).

Assertions have two purposes: they help with documentation and can also catch
programming errors.

For assertions, use

log_halt("message", is_assertion=.true.).


#### More documentation guidelines

Some guidelines inspired in Agile modelling documentation guidelines
(http://www.agilemodeling.com/essays/agileDocumentation.htm):

- Explain what the code does in the `.f90` files, preferrably as close
  to the actual code as possible.
- Keep the information in the `.md` files to a minimum, mostly to give
  directions to users who just bumped into the project.


#### Commenting .f, .f90 files

*What to put in comments*. It is recommended to document at least this:

- Explain -- at the beginning of a module -- what the prefixes of variables declared within it stand for.
- `module`, `subroutine`, and `function`: at least one description line .
- `subroutine` or `function` arguments: at least one sentence for each argument.
- variables declared in the header section of a module: at least one sentence.
- when the logic becomes tricky, it is a kind gesture to explain what the code is doing

#### Tags

These are search keywords with the following meaning (this list is probably not complete):
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

Tags are case-insensitive.
 
 
## Development how-to's
 
This section contains guidelines for implementing new features in the source code

### How to create a new command-line option

To add a new command-line option to the Fortran code:

:one: Open `config.f90`.

:two: Find string `module config`.

:three: Create a new variable starting with `config_` and set its default value. :bulb:

:four: In subroutine `init_options()`, add a `call add_option(...)`. :bulb:

:five: In subroutine `handle_option`, add a `case` for your new variable. :bulb:

:six: Finally, use your new `config_xxxx` variable in other modules of the source code,
   making sure that the module that you are working on has a `use config` statement 
   at its beginning.
         
Python code: steps 7-10 are required to make `pyfant` aware of the new option.     

:seven:. open `PFANT/pyfant/pyfant/conf.py`.

:eight:. Add `self.xxxx = None` inside `Options.__init__()` where `xxxx` is the name of your new
   option, without the config_ prefix.

To make the new option editable Options editor that appears in the `x.py` Graphical
user interface (GUI):

:nine: Open `PFANT/pyfant/pyfant/gui/a_WOptionsEditor.py`

:ten: :one: :zero: You will need to make a few interventions inside the `__init__()` method,
which should become clear from the existing code. 


