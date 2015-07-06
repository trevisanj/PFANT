/**
@page github Working with the GitHub repository

Cloning the repository
======================

The repository has two branches:
@li @c master All Python and Fortran source code
@li @c gh-pages PFANT website, including Doxygen-generated documentation

If you are not bothered about re-compiling the documentation with Doxygen, just clone the
repository:

@code
git clone https://github.com/trevisanj/pfant
@endcode

Setting up to re-compile the documentation
------------------------------------------

When working on your computer locally, in order to have Doxygen parsing files from the
@c master branch while generating output directly into
the @c gh-pages branch, it is necessary to work with the *two branches of the repository
as parallel local directories*. This is achieved by invoking <code>git clone</code>
<b>--single-branch</b></code> option twice.

@note Cloning only one branch is not the default behaviuor of git. The default is to
clone all the branches and then switch between branches with the <code>git checkout</code>
command. You can still work on the code (or web pages) like this if you want, but you
won't be able to use Doxygen as expected.

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

References
==========

https://help.github.com/articles/generating-ssh-keys/

https://help.github.com/articles/changing-a-remote-s-url/

*/
