#!/bin/sh
# Run this to generate all the initial makefiles, etc.

PKG_NAME=cashutil
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

DIE=0

INTLTOOLIZE=${INTLTOOLIZE:-intltoolize}
LIBTOOLIZE=${LIBTOOLIZE:-libtoolize}
LIBTOOL=${LIBTOOL:-libtool}

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`autoconf' installed to compile $PKG_NAME."
  echo "Download the appropriate package for your distribution or get the source."
  echo "Get ftp://ftp.gnu.org/pub/gnu/autoconf/autoconf-2.50.tar.gz"
  echo "(or a newer version if it is available)"
  DIE=1
}

(grep "^AM_PROG_LIBTOOL" $srcdir/configure.ac >/dev/null) && {
  (glibtool --version) < /dev/null > /dev/null 2>&1 ||
  (libtool --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "**Error**: You must have \`libtool' installed to compile $PKG_NAME."
    echo "Get ftp://ftp.gnu.org/pub/gnu/libtool/libtool-1.5.tar.gz"
    echo "(or a newer version if it is available)"
    DIE=1
  }
}

(grep "^AC_PROG_INTLTOOL" $srcdir/configure.ac >/dev/null) && {
  (intltoolize --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "**Error**: You must have \`intltool' installed."
    echo "You can get it from:"
    echo "  ftp://ftp.gnome.org/pub/GNOME/"
    DIE=1
  }
}

(grep "^AM_GLIB_GNU_GETTEXT" $srcdir/configure.ac >/dev/null) && {
  (grep "sed.*POTFILES" $srcdir/configure.ac) > /dev/null || \
  (glib-gettextize --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "**Error**: You must have \`glib' installed."
    echo "You can get it from: ftp://ftp.gtk.org/pub/gtk"
    DIE=1
  }
}

# usage: test_version program version
# returns 0 if program >= version; returns 1 if not.
test_version()
{
    this_prog="$1"
    want_vers="$2"

    testv=`"$this_prog" --version 2>/dev/null | head -1 | awk '{print $NF}'`
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

# These statics should really be defined at the top
find_program "$AUTOCONF" autoconf "2.53"
AUTOCONF="$program"
find_program "$AUTOHEADER" autoheader "2.53"
AUTOHEADER="$program"
find_program "$AUTOMAKE" automake "1.5 1.6 1.7 1.8"
AUTOMAKE="$program"
find_program "$ACLOCAL" aclocal "1.5 1.6 1.7 1.8"
ACLOCAL="$program"

($AUTOMAKE --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`automake' installed to compile $PKG_NAME."
  echo "Get ftp://ftp.gnu.org/pub/gnu/automake/automake-1.7.tar.gz"
  echo "(or a newer version if it is available)"
  DIE=1
  NO_AUTOMAKE=yes
}

# if no automake, don't bother testing for aclocal
test -n "$NO_AUTOMAKE" || (aclocal --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: Missing \`aclocal'.  The version of \`automake'"
  echo "installed doesn't appear recent enough."
  echo "Get ftp://ftp.gnu.org/pub/gnu/automake/automake-1.7.tar.gz"
  echo "(or a newer version if it is available)"
  DIE=1
}

if test "$DIE" -eq 1; then
  exit 1
fi

if test -z "$*"; then
  echo "**Warning**: I am going to run \`configure' with no arguments."
  echo "If you wish to pass any to it, please specify them on the"
  echo \`$0\'" command line."
  echo
fi

case $CC in
xlc )
  am_opt=--include-deps;;
esac

for coin in `find $srcdir -name configure.ac -print`
do
  dr=`dirname $coin`
  if test -f $dr/NO-AUTO-GEN; then
    echo skipping $dr -- flagged as no auto-gen
  else
    echo processing $dr
    ( cd $dr
      aclocalinclude="$ACLOCAL_FLAGS -I m4"

      if grep "^AM_GLIB_GNU_GETTEXT" configure.ac >/dev/null; then
	if grep "sed.*POTFILES" configure.ac >/dev/null; then
	  : do nothing -- we still have an old unmodified configure.ac
	else
	  echo "Creating $dr/aclocal.m4 ..."
	  test -r $dr/aclocal.m4 || touch $dr/aclocal.m4
	  echo "Running glib-gettextize...  Ignore non-fatal messages."
	  echo "no" | glib-gettextize --force --copy
	  echo "Making $dr/aclocal.m4 writable ..."
	  test -r $dr/aclocal.m4 && chmod u+w $dr/aclocal.m4
        fi
      fi
      if grep "^AM_PROG_LIBTOOL" configure.ac >/dev/null; then
	if test -z "$NO_LIBTOOLIZE" ; then
	    case "$OSTYPE" in
		*darwin*)
		    echo "Running glibtoolize... ($MACHTYPE)"
		    glibtoolize --force --copy
		;;
		    *)
		    echo "Running libtoolize..."
		    libtoolize --force --copy
		;;
	    esac
	fi
      fi
     if grep "^AC_PROG_INTLTOOL" configure.ac >/dev/null; then
        echo "Running intltoolize ..."
        intltoolize --force --copy --automake
      fi

      echo "Running aclocal $aclocalinclude ..."
      $ACLOCAL $aclocalinclude || {
	echo
	echo "**Error**: aclocal failed. This may mean that you have not"
	echo "installed all of the packages you need, or you may need to"
	echo "set ACLOCAL_FLAGS to include \"-I \$prefix/share/aclocal\""
	echo "for the prefix where you installed the packages whose"
	echo "macros were not found"
	exit 1
      }

      if grep "^AM_CONFIG_HEADER" configure.ac >/dev/null; then
	echo "Running autoheader..."
	$AUTOHEADER || { echo "**Error**: autoheader failed."; exit 1; }
      fi
      echo "Running automake --gnu $am_opt ..."
      $AUTOMAKE --add-missing --gnu $am_opt ||
	{ echo "**Error**: automake failed."; exit 1; }
      echo "Running autoconf ..."
      $AUTOCONF || { echo "**Error**: autoconf failed."; exit 1; }
    ) || exit 1
  fi
done

conf_flags="--enable-maintainer-mode " #--enable-compile-warnings --enable-iso-c

if test x$NOCONFIGURE = x; then
  echo Running $srcdir/configure $conf_flags "$@" ...
  $srcdir/configure $conf_flags "$@" \
  && echo Now type \`make\' to compile $PKG_NAME || exit 1
else
  echo Skipping configure process.
fi
