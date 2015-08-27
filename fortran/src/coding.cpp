/**

@page coding_guide Coding guide

@subpage coding_style Coding style manual

@note The symbol @c % refers to the root directory of the PFANT repository.

@section github Working with the GitHub repository

@subsection github_clone Cloning the repository

@code
git clone https://github.com/trevisanj/pfant
@endcode


@subsection ssh_setup Communicating with GitHub <i>via</i> SSH

To keep git from asking for your github username and password every time you execute a
<code>git push</code>, you have to configure git to use the SSH protocol to communicate
with github.

The steps involved are roughly summarized below. For full details, please follow the
tutorials referenced below.
<ol>

  <li>Generate a SSH key. I followed the tutorial at
  https://help.github.com/articles/generating-ssh-keys/,
  but the commands are roughly as follows:
  @code
  $ ls -al ~/.ssh  # Check existing SSH keys
  $ sh-keygen -t rsa -C "your@email.com"
  $ ssh-add ~/.ssh/id_rsa
  $ pluma  ~/.ssh/id_rsa.pub
  @endcode
  </li>

  <li>Copy-paste the key to a box at a settings page at GitHub.
  After you do this, you can test if it works:
  @code
  $ ssh -T git@github.com
  @endcode
  </li>

  <li>Change the remote URL that git uses. Tutorial at
  https://help.github.com/articles/changing-a-remote-s-url/
  @code
  $ git remote set-url origin git@github.com:trevisanj/pfant.git
  @endcode
  </li>
</ol>

@section cbfortran CodeBlocks Fortran (CBFortran)

CBFortran is a customization of the "Code Blocks" IDE optimized for Fortran.

This tool is recommended to work with Fortran projects (such as PFANT) because
it can generate Makefile's and compile the source code with little pain.

Project site: http://cbfortran.sourceforge.net/

@subsection cbfortran_linux Running CBFortran on Linux

CodeBlocks Fortran does not require installation. Just download the .taz.bz2 file from the
downloads page http://cbfortran.sourceforge.net/downloads.html and extract it into your
home directory.

@attention In order to run, it must be extracted into your home directory (will create
directory <code>/home/user/CodeBLocks_Fortran_xxxxxxxx</code>).

After extracted, enter directory <code>/home/user/CodeBLocks_Fortran_xxxxxxxx</code> and
execute <code>codeblocks_run.sh</code>

@subsection cbfortran_work Using CBFortran

The CBFortran project is the file %/fortran/PFANT.cbp.

Some shortcuts:
@li Find in files: Ctrl+Shift+F
@li Compile: Ctrl+F9
@li Run: Ctrl+F10



@section coding_doxygen Working with Doxygen

Doxygen is used to generate HTML documentation such as this.

To recompile the HTMLs, run @c doxygen inside <code>%/fortran</code>.
This will generate output inside <code>%/gh-pages/doxy-fortran</code>

To push this documentation online, simply run the script

<code>deploy_doxy-fortran.sh</code> inside directory <code>%</code>

@subsection doxy_troubleshooting Troubleshooting doxygen compilation

@subsection segfault_doxygen Segmentation fault running doxygen

*Doxygen has got Fortran code parsing bugs*. I don't know the exact situation that causes
doxygen to crash, but I have figured out things to be avoided.

@par Backslashes
Doxygen crashed because of the following line of code:

@code
      if (wdir_trim(i:i) .eq. '\') wdir_trim(i:i) = '/'
@endcode

Besides, the file %/fortran/Doxyfile must have

@code
ENABLE_PREPROCESSING   = NO
@endcode





@section coding_ref References

https://help.github.com/articles/generating-ssh-keys/

https://help.github.com/articles/changing-a-remote-s-url/

http://cbfortran.sourceforge.net/

*/