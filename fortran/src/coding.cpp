/**

@page coding_guide Coding guide



@section github Working with the GitHub repository

@subsection github_clone Cloning the repository

@subsubsection github_clone1 Most cases

@code
git clone https://github.com/trevisanj/pfant
@endcode

@subsubsection github_clone0 IF you want to run doxygen

The repository has two branches:
@li @c master All Python and Fortran source code
@li @c gh-pages PFANT website, including Doxygen-generated documentation

When working on your computer locally, in order to have Doxygen parsing files from the
@c master branch while generating output directly into
the @c gh-pages branch, it is necessary to work with the *two branches of the repository
as parallel local directories*. This is achieved by invoking <code>git clone</code>
<b>--single-branch</b></code> option twice.

Here is a set of commands to fetch the two branches separately:

@code
# It is good to be inside a directory called "github" on your computer
$ pwd
/home/j/Documents/projects/pfant/github

# This will clone the "master" branch into a directory called "master"
$ git clone https://github.com/trevisanj/pfant --branch master --single-branch master

# This will clone the "gh-pages" branch into a directory called "gh-pages"
$ git clone https://github.com/trevisanj/pfant --branch gh-pages --single-branch gh-pages

$ ls
gh-pages  master
@endcode

Communicating with GitHub <i>via</i> SSH
========================================

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


















/**

@page codeblocks CodeBlocks Fortran

CodeBlocks Fortran is a great IDE for working with Fortran projects.

@note Install "CodeBlocks Fortran", @b not "CodeBlocks".

Project site: http://cbfortran.sourceforge.net/

Running on Linux
================

CodeBlocks Fortran does not require installation. Just download the .taz.bz2 file from the
downloads page http://cbfortran.sourceforge.net/downloads.html and extract it into your
home directory.

@attention In order to run, it must be extracted into your home directory (will create
directory <code>/home/user/CodeBLocks_Fortran_xxxxxxxx</code>).

After extracted, enter directory <code>/home/user/CodeBLocks_Fortran_xxxxxxxx</code> and
execute <code>codeblocks_run.sh</code>

Working with the PFANT Project
==============================

In CodeBlocks Fortran, open file <code>main.cbp</code>, which is in the <code>main</code>
directory of the master branch.

Some shortcuts
--------------
@li Find in files: Ctrl+Shift+F
@li Compile: Ctrl+F9
@li Run: Ctrl+F10

*/




































@subpage codingStyle

@subpage github

@subpage codeblocks

@subpage doxygen

/*
@page troubleshooting Troubleshooting

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





References
==========

https://help.github.com/articles/generating-ssh-keys/

https://help.github.com/articles/changing-a-remote-s-url/

*/