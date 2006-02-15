#!/bin/sh
# Run this to generate all the initial makefiles, etc.

echo CashUtil development within gnucash has halted. 
echo It may or may not be revived after gnucash 2.0.0 - 
echo development has restarted outside gnucash, see
echo http://www.linux.codehelp.co.uk/cashutil/
exit 1

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="gnucash"

(test -f $srcdir/configure.ac \
## put other tests here
) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

USE_GNOME2_MACROS=1
. $srcdir/macros/autogen.sh

