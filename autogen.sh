#!/bin/sh -x

# Exit this script if any command fails with non-zero exit status.
set -e

# First cache the command names in variables. If you want to
# override the names, simply set the variables before calling this
# script.

: ${GLIB_GETTEXTIZE=glib-gettextize}
: ${INTLTOOLIZE=intltoolize}
: ${LIBTOOLIZE=libtoolize}
: ${ACLOCAL=aclocal}
: ${AUTOHEADER=autoheader}
: ${AUTOMAKE=automake}
: ${AUTOCONF=autoconf}

${GLIB_GETTEXTIZE} --force
${INTLTOOLIZE} --force
${LIBTOOLIZE} --force --automake
${ACLOCAL} -I macros ${ACLOCAL_FLAGS}
${AUTOHEADER}
${AUTOMAKE} --add-missing
${AUTOCONF}

echo "Now you can run ./configure --enable-maintainer-mode --enable-error-on-warning --enable-compile-warnings"
