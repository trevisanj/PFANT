#!/usr/bin/python
"""
Adds lines to ~/.bashrc file or other specified + appends paths to environment variables

**Note** Will work only on if os.name == "posix"

"""

import os
import argparse
import sys

def print2(s):
  """function to standarde message lines."""
  print '--> %s' % s


if __name__ == "__main__":
  if os.name != "posix":
    print "OS is '"+os.name+"', this script is only for 'posix' OS's, sorry."
    sys.exit()

  home = os.getenv("HOME")

  DEF_TARGET = '(home)/.bashrc or (home)/.cshrc'

  parser = argparse.ArgumentParser(
    description='Adds lines to ~/.bashrc file or other specified',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  parser.add_argument('target', type=str, help='full path to destination',
                      default=DEF_TARGET, nargs='?')
  parser.add_argument('--bash', help='bash mode', action="store_true")
  parser.add_argument('--tcsh', help='tcsh mode',action="store_true")

  args = parser.parse_args()

  # Validation additional to parse_args()
  if not args.bash and not args.tcsh:
    parser.error("Either set --bash or --tcsh.")
  if args.bash and args.tcsh:
    parser.error("Set either --bash or --tcsh, not both")

  # Shell-dependent configuration
  if args.bash:
    print2("Running in bash mode")
    path_var = "PATH"
    fn_conf = ".bashrc"
    get_cmd0 = lambda p: 'export PYTHONPATH="${PYTHONPATH}:%s"' % p
    get_cmd1 = lambda p: 'export PATH="${PATH}:%s"' % p
  else:
    print2("Running in tcsh mode")
    path_var = "path"
    fn_conf = ".cshrc"
    get_cmd0 = lambda p: 'setenv PYTHONPATH "{$PYTHONPATH}:%s"' % p
    get_cmd1 = lambda p: 'set path = ($path %s)' % p

  # file to be changed/created
  t = args.target if args.target is not DEF_TARGET else os.path.join(home, fn_conf)
  # directory containing this script ("root" directory of PFANT repository)
  p = os.path.dirname(os.path.realpath(__file__))

  map_ = [
    ("PYTHONPATH", "%s/pyfant"% p, get_cmd0),
    (path_var, "%s/pyfant/scripts" % p, get_cmd1),
    (path_var, "%s/fortran/bin" % p, get_cmd1)]

  ll = [f(p) for v, p, f in map_]  # list of commands

  if os.path.isfile(t):
    to_add = []
    with open(t, 'r') as f:
      orig = f.read()+"\n"

      for l in ll:
        if l not in orig:
          #print "Adding >", l
          to_add.append(l)
  else:
    orig = ""
    to_add = ll

  if len(to_add) > 0:
    with open(t, 'w') as f:
      s = orig+("\n".join(["# added by add-paths.py (PFANT)\n"+x for x in to_add]))
      f.write(s)

    print2("Adding the following lines to file '%s')" % t)
    print "***BEGIN***"
    for x in to_add:
      print x
    print "***END***"




      # print "File '%s' now contains:\n--- BEGIN ---" % t
      # print s
      # print "--- END ---"
  else:
    print2("No changes made to file '%s'" % t)

  # Runs another shell that will parse its new configuration file
  if args.bash:
    print2("Running another bash...")
    os.system("bash")
  else:
    print2("Running another tcsh")
    os.system("tcsh")

