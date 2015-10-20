/**

@page coding_tools Coding tools

@subpage coding_intro Introduction

This document describes the use of a few tools that help dealing with the source code.
Some are the softwares that have been used
@li Code Blocks Fortran (CBFortran)  # compile, find in files (Ctrl+Shift+F)
@li PyCharm  # column mode (Alt+Shift+Insert); search-and-replace in files (Ctrl+Shift+R)
@li Eclipse IDE with Eclox and ?doc? C++ plugin [find name]  # editing C++ comments for Doxygen
@li subl (Sublime Text Editor)  # great Fortran syntax highlighting (plugin install required)
@li EMACS
@li meld <filename1> <filename2>  # compares two files

The use of a few of these is described below in further detail.
Legend
@verbatim
@endverbatim





@section coding_github Working with the github repository
 
To clone the github repository:
 
@code
 git clone https://github.com/trevisanj/PFANT
 @endcode

This will create a directory named PFANT, which is treated as our root directory in this
document.

@subsection ssh_setup Communicating with github <i>via</i> SSH

This will facilitate future <code>git push</code> invokes.
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
  $ ssh-keygen -t rsa -C "your@email.com"
  $ ssh-add ~/.ssh/id_rsa
  $ <your_text_editor>  ~/.ssh/id_rsa.pub
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
 
 
@section coding_doxygen Working with Doxygen
 
Doxygen is used to generate HTML documentation such as this.

To recompile the HTMLs, run @c doxygen inside <code>PFANT/fortran</code>.
This will generate output inside <code>PFANT/gh-pages/doxy-fortran</code>
 
To push this documentation online, simply run the script
 
<code>deploy_doxy-fortran.sh</code> inside directory <code>PFANT</code>
 
@subsection doxy_troubleshooting Troubleshooting doxygen compilation
 
@subsection segfault_doxygen Segmentation fault running doxygen
 
*Doxygen has got Fortran code parsing bugs*. I don't know the exact situation that causes
doxygen to crash, but I have figured out things to be avoided.

@par Backslashes
Doxygen crashed because of the following line of code:
 
@code
      if (wdir_trim(i:i) .eq. '\') wdir_trim(i:i) = '/'
@endcode
 
Besides, the file PFANT/fortran/Doxyfile must have
 
@code
ENABLE_PREPROCESSING   = NO
@endcode
 
 
 
 
 
 
 
 
@section cbfortran CodeBlocks Fortran (CBFortran)
 
CBFortran is a customization of the
Code Blocks (CB) IDE optimized for working with Fortran projects.
 
 
This IDE provides an easy way to compile the code and generate makefiles. CB was used to 
create the makefiles in the
PFANT/fortran directory.
 
@subsection cbfortran_linux Running CBFortran on Linux
 
CBFortran does not require installation. Just download the .taz.bz2 file from the
downloads page http://cbfortran.sourceforge.net/downloads.html and extract it into your
home directory (will create
directory <code>/home/user/CodeBLocks_Fortran_xxxx</code>).
 
After extracted, enter the new directory, and
execute <code>codeblocks_run.sh</code>
 
@subsection cbfortran_work Using CBFortran
 
The CBFortran project is the file PFANT/fortran/PFANT.cbp.
 
Some shortcuts:
@li Find in files: Ctrl+Shift+F
@li Compile: Ctrl+F9
@li Run: Ctrl+F10
 
 
@section coding_ref References
 
https://help.github.com/articles/generating-ssh-keys/
 
https://help.github.com/articles/changing-a-remote-s-url/
 
http://cbfortran.sourceforge.net/
*/
