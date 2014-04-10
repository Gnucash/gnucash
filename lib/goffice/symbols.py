#!/usr/bin/env python

"""
jsled, 2005 -- munge nm -a output to get a listing of actually undefined
  symbols in libgoffice.

USAGE:
[jsled@phoenix:~/../gnucash/lib/goffice]$ nm -a .libs/libgoffice.a | ./symbols.py | egrep -v "(xml|gtk_|g_|gdk_|pango_|gsf_|art_|gnome_|glade_)" | sort

"""

import sys, re

def main():
    syms = {}
    for line in sys.stdin:
        line = line.strip()
        match = re.match( r".*\b(U|T|D|c|t|R|B|C) ([^ ]+).*", line )
        if not match:
            continue
        sym = match.group(2)
        type = match.group(1)
        if not syms.has_key( sym ):
            syms[sym] = type
        elif syms[sym] == "U":
            syms[sym] = type
    for sym,status in syms.iteritems():
        if status == "U":
            print sym

if __name__ == "__main__":
    main();
