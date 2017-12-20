#!/usr/bin/env python

"""Parses pfant help to make a command-line statement containing all the arguments and their default values

This should be included in the cheatcheet
"""

import re
import subprocess

FN_IN = "helpdump.txt"
FN_OUT = "commands.rst"
FROM_MAIN = ' (read from main configuration file)'

def fmt_h3(s):
    return ["", "``{}``".format(s), "~"*(len(s)+4), ""]

def main():
    ll = []
    for exe in ["hydro2", "innewmarcs", "pfant", "nulbad"]:
        help = subprocess.check_output([exe, "--help"]).decode("utf8").replace("\n", "\\")
        matches = re.findall("--(\w+).*?\[\=(.+?)\]", help)
        ll.append("\n".join(fmt_h3(exe)+[".. code:: shell", "", ""])+" \\\n    ".join(["    "+exe]+["    --{} {}".format(arg, default.replace(FROM_MAIN, "")) for arg, default in matches if arg != "help"]))

    text = "\n".join(ll)
    print(text)
    print("--------------------------------------")
    with open(FN_OUT, "w") as h:
        h.write(text)
    print("Saved file {}".format(FN_OUT))


if __name__ == "__main__":
    main()