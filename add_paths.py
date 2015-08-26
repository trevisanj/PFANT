#!/usr/bin/python
"""
Adds lines to ~/.bashrc file or other specified + appends paths to environment variables

Will work only on if os.name == "posix"

Note: only works with the bash shell.

"""

import os
import argparse


if __name__ == "__main__":
  if os.name != "posix":
    print "OS is '"+os.name+"', this script is only for 'posix' OS's, sorry."

  home = os.getenv("HOME")
  t = os.path.join(home, '.bashrc')


  parser = argparse.ArgumentParser(
    description='Adds lines to ~/.bashrc file or other specified',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  parser.add_argument('target', type=str, help='full path to destination',
                      default=t, nargs='?')

  args = parser.parse_args()

  if args.target is not None:
    t = args.target

  p = os.path.dirname(os.path.realpath(__file__))


  map_ = [
    ("PYTHONPATH", "%s/pypfant"% p),
    ("PATH", "%s/pypfant/bin" % p),
    ("PATH", "%s/fortran/bin" % p)]

  get_cmd = lambda v, p: 'export %s="${%s}:%s"' % (v, v, p)

  ll = [get_cmd(v, p) for v, p in map_]

  # ll = ["""export PYTHONPATH="${PYTHONPATH}:%s/pypfant" """ % p,
  #       """export PATH="${PATH}:%s/pypfant/bin" """ % p,
  #       """export PATH="${PATH}:%s/fortran/bin" """ % p,
  #       ]

  os.environ['APARA'] = 'qqqqqq'

  if os.path.isfile(t):
    to_add = []
    with open(t, 'r') as f:
      orig = f.read()+"\n"

      for l in ll:
        if l not in orig:
          print "Adding >", l
          to_add.append(l)
  else:
    orig = ""
    to_add = ll

  if len(to_add) > 0:
    with open(t, 'w') as f:
      s = orig+("\n".join(["# added by add_paths.py (PFANT)\n"+x for x in to_add]))
      f.write(s)

      print "File '%s' now contains:\n--- BEGIN ---" % t
      print s
      print "--- END ---"
  else:
    print "No changes made to file '%s'" % t

  # runs commands
  any_ = False
  for v, p in map_:
    goes = False
    try:
      e = os.environ[v]

      if not p in e:
        goes = True
    except KeyError:
      print "Fffff"
      goes = True

    if goes:
      cmd = get_cmd(v, p)
      print "running > "+cmd
      os.system(cmd)
      any_ = True

  if any_:
    print "Running another bash"
    os.system("bash")

