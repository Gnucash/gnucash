#!/bin/sh
# Run this to generate all the initial makefiles, etc.

if test -z "$srcdir" ; then srcdir=`dirname $0` ; fi
if test -z "$srcdir" ; then srcdir=.            ; fi
cd $srcdir

DIE=0

if [ -n "${GNOME2_PATH}" ]; then
	for dir in `echo ${GNOME2_PATH} | sed 's/:/ /g'`; do
	    if test -d "${dir}/share/aclocal"; then
		ACLOCAL_FLAGS="${ACLOCAL_FLAGS} -I ${dir}/share/aclocal"
	    fi;
	    if test -d "${dir}/bin"; then
		PATH="${PATH}:${dir}/bin"
	    fi;
	done;
	export PATH
fi

# usage: test_version program version
# returns 0 if program >= version; returns 1 if not.
test_version()
{
    this_prog="$1"
    want_vers="$2"

    testv=`"$this_prog" --version 2>/dev/null | head -n 1 | awk '{print $NF}'`
    if test -z "$testv" ; then return 1 ; fi

    testv_major=`echo "$testv" | sed 's/\([0-9]*\).\([0-9]*\).*$/\1/'`
    testv_minor=`echo "$testv" | sed 's/\([0-9]*\).\([0-9]*\).*$/\2/'`

    vers_major=`echo "$want_vers" | sed 's/\([0-9]*\).\([0-9]*\).*$/\1/'`
    vers_minor=`echo "$want_vers" | sed 's/\([0-9]*\).\([0-9]*\).*$/\2/'`

    # if wanted_major > found_major, this isn't good enough
    if test $vers_major -gt $testv_major ; then
	return 1
    # if wanted_major < found_major, then this is fine
    elif test $vers_major -lt $testv_major ; then
	return 0
    # if we get here, then the majors are equal, so test the minor version
    # we want found_minor >= want_minor.
    # So, if want_minor > found_minor, this is bad.
    elif test $vers_minor -gt $testv_minor ; then
	return 1
    # this is it.
    else
	return 0
    fi
}

# usage: find_program preset program version "<other versions>"
# sets "program" to the name of the program to use.
# if preset is set, then use that regardless,
#  otherwise check if "program" is of a good enough version and use that,
#  otherwise check if "program-version" is of a good enough version and use that.
#  otherwise return an error.
find_program()
{
    find="$1"
    prog="$2"
    vers="$3"
    extravers="$4"

    if test -n "$find" ; then
	test_version "$find" "$vers"
	status="$?"
	if test "$status" = 0 ; then
	    program="$find"
	    return 0
	fi
	echo "**Error**: cannot use $find"
    else

	test_version "$prog" "$vers"
	status=$?
	if test "$status" = 0 ; then
	    program="$prog"
	    return 0
	fi

	for test_vers in $vers $extravers ; do
	    test_version "$prog-$test_vers" "$vers"
	    status=$?
	    if test "$status" = 0 ; then
		program="$prog-$test_vers"
		return 0
	    fi
	done
    fi

    echo
    echo "**Warning**: Could not find a $prog that identifies itself >= $vers."
    echo
    program="$prog"
}

############################################################
#
# Store all required programs in variables. All of these variables
# could have been set by the developer beforehand, if he/she needs to
# override the defaults of here.

: ${GLIB_GETTEXTIZE=glib-gettextize}
: ${INTLTOOLIZE=intltoolize}
: ${LIBTOOLIZE=libtoolize}

find_program "$AUTOCONF" autoconf 2.59
AUTOCONF="$program"
find_program "$AUTOHEADER" autoheader 2.59
AUTOHEADER="$program"
find_program "$AUTOMAKE" automake 1.9
AUTOMAKE="$program"
find_program "$ACLOCAL" aclocal 1.9
ACLOCAL="$program"

############################################################
#
# All variables are set. Now check whether these programs are really
# available.

(${GLIB_GETTEXTIZE} --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`glib-gettextize' installed to compile GnuCash."
  echo "Get the development packages of the glib-2.x library from your distribution."
  DIE=1
}

(${INTLTOOLIZE} --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`intltoolize' installed to compile GnuCash."
  echo "Get the package 'intltool' of your distribution."
  DIE=1
}

# On MacOS, libtoolize is installed as glibtoolize, so handle that here.
(${LIBTOOLIZE} --version) < /dev/null > /dev/null 2>&1 || {
  LIBTOOLIZE=glibtoolize
  (${LIBTOOLIZE} --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "**Error**: You must have \`libtoolize' installed to compile GnuCash."
    echo "Could not find either \`libtoolize' or \'glibtoolize'."
    echo "Download the appropriate package for your distribution,"
    echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
    DIE=1
  }
}

(${AUTOMAKE} --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`automake' installed to compile GnuCash."
  echo "Download the appropriate package for your distribution,"
  echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
  NO_AUTOMAKE=yes
}


# if no automake, don't bother testing for aclocal
test -n "$NO_AUTOMAKE" || (${ACLOCAL} --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: Missing \`aclocal'.  The version of \`automake'"
  echo "installed doesn't appear recent enough (older than automake-1.4)."
  echo "Download the appropriate package for your distribution,"
  echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
}

(${AUTOCONF} --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`autoconf' installed to compile GnuCash."
  echo "Download the appropriate package for your distribution,"
  echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
}

if test "$DIE" -eq 1; then
  exit 1
fi

case $CC in
xlc )
  am_opt=--include-deps;;
esac

############################################################
#
# All programs are available. So now actually call them in the
# required order.

echo "Creating po/POTFILES.in ..."
test -r po/POTFILES.in || touch po/POTFILES.in

echo "Creating aclocal.m4 ..."
test -r aclocal.m4 || touch aclocal.m4

echo "Running ${GLIB_GETTEXTIZE} --force --copy ...  "
echo "GnuCash note: Please ignore the output of ${GLIB_GETTEXTIZE} below!"
echo "no" | ${GLIB_GETTEXTIZE} --force --copy
echo "GnuCash note: Please ignore the output of ${GLIB_GETTEXTIZE} above!"
echo

echo "Ensure aclocal.m4 is writable ..."
test -r aclocal.m4 && chmod u+w aclocal.m4

echo "Ensure po/POTFILES.in is writable ..."
test -r po/POTFILES.in && chmod u+w po/POTFILES.in

echo "Running ${INTLTOOLIZE} --force --copy ..."
${INTLTOOLIZE} --force --copy || \
    { echo "**Error**: ${INTLTOOLIZE} failed."; exit 1; }

echo "Running ${LIBTOOLIZE} --force --copy ..."
${LIBTOOLIZE} --force --copy || \
    { echo "**Error**: ${LIBTOOLIZE} failed."; exit 1; }

aclocalinclude="$ACLOCAL_FLAGS -I macros"
echo "Running ${ACLOCAL} $aclocalinclude ..."
${ACLOCAL} $aclocalinclude || \
    { echo "**Error**: ${ACLOCAL} failed."; exit 1; }

echo "Running ${AUTOHEADER}..."
${AUTOHEADER} || { echo "**Error**: ${AUTOHEADER} failed."; exit 1; }

echo "Running ${AUTOMAKE} --add-missing --gnu --warnings=no-portability $am_opt ..."
${AUTOMAKE} --add-missing --gnu --warnings=no-portability $am_opt || \
    { echo "**Error**: ${AUTOMAKE} failed."; exit 1; }

echo "Running ${AUTOCONF} ..."
${AUTOCONF} || \
    { echo "**Error**: ${AUTOCONF} failed."; exit 1; }

############################################################
# Done.

conf_flags="--enable-compile-warnings" # --enable-iso-c --enable-error-on-warning

echo ""
echo "NOTE: Just run configure.  Even if something told you to run"
echo "      aclocal, automake, or anything else above, IGNORE IT."
echo "      Everything has been run properly.  Just run configure..."
echo ""
echo You must now run $srcdir/configure $conf_flags "$@" ...
