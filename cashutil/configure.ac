# This is free software, licensed under the GNU Public License V2.
# See the file COPYING for details.

AC_INIT(cashutil, 0.1.0, http://lists.sourceforge.net/lists/listinfo/qof-devel)

AM_MAINTAINER_MODE
AC_PREFIX_DEFAULT(/usr/local)
AC_PREREQ(2.53)
AM_CONFIG_HEADER(config.h)

dnl ******************************
dnl CashUtil Version
dnl ******************************
CASHUTIL_VERS=0
CASHUTIL_MAJOR=1
CASHUTIL_MINOR=0

AC_SUBST(CASHUTIL_VERS)
AC_SUBST(CASHUTIL_MAJOR)
AC_SUBST(CASHUTIL_MINOR)
VERSION="$CASHUTIL_VERS.$CASHUTIL_MAJOR.$CASHUTIL_MINOR"

AC_CANONICAL_HOST
AC_CANONICAL_SYSTEM
AC_DEFINE_UNQUOTED(HOST_OS, "$host", [Host type])
AM_INIT_AUTOMAKE(cashutil, [$VERSION])
AC_PROG_INTLTOOL
INTLTOOLIZE=${INTLTOOLIZE:-intltoolize}

dnl ******************************
dnl Checks for basic programs.
dnl ******************************
AC_PROG_CC
AC_PROG_INSTALL
AM_PROG_LIBTOOL
AC_PROG_YACC

dnl ******************************
dnl Platform-specific things
dnl ******************************

dnl PIC_LIBS is flags needed to compile PIC, for shared libs
dnl where some linker offsets are not allowed. Currently set
dnl for FreeBSD-amd64 only.
PIC_LIBS=""

dnl null_device is the default NULL device on your system
dnl (usually /dev/null or NUL). If yours is _not_ /dev/null,
dnl set it in the platform-specific section below.
null_device="/dev/null"

case "$host" in
        *darwin*)
        dnl Use fink under MacOS X
        AC_MSG_CHECKING(for fink support)
        if test -d "/sw/lib" -a -d "/sw/include"; then
            CPPFLAGS="$CPPFLAGS -I/sw/include"
            LDFLAGS="$LDFLAGS -L/sw/lib"
            AC_MSG_RESULT(yes)
        else
            AC_MSG_RESULT(no)
        fi
        AC_CHECK_HEADERS(popt.h)
        AC_MSG_RESULT([yes, patching libtool to always build dylibs])
        mv libtool libtool.old
        sed -e 's/^deplibs_check_method.*/deplibs_check_method=pass_all/g' \
            -e 's|^archive_cmds.*|archive_cmds="$CC -dynamiclib \\$allow_undefined_flag -o \\$lib \\$libobjs \\$deplibs\\$linker_flags -install_name \\$rpath/\\$soname \\$verstring"|g' \
            -e 's|^library_names_spec.*|library_names_spec="\\$libname\\$release\\$versuffix.dylib \\$libname\\$release\\${major}.dylib \\$libname.dylib"|g' \
            -e 's|^soname_spec.*|soname_spec="\\$libname\\$release\\$major.dylib"|g' \
        < libtool.old > libtool
        rm libtool.old
        AC_MSG_RESULT([yes, patching libtool to avoid the libtool bug on MacOSX])
        mv libtool libtool.old
        sed -e 's|^old_archive_cmds.*|old_archive_cmds="\\$AR \\$AR_FLAGS \\$oldlib\\$oldobjs\\$old_deplibs \&\& \\$RANLIB \\$oldlib"|g' \
            -e 's|^old_postinstall_cmds.*|old_postinstall_cmds="\\$RANLIB \\$oldlib \&\& chmod 644 \\$oldlib"|g' \
       < libtool.old > libtool
        rm libtool.old
    ;;
        amd64*freebsd*)
                dnl Need -fPIC for shared libs
                PIC_LIBS="-fPIC"
    ;;
        *solaris*)
                dnl Check if we need -lresolv for inet_pton
                AC_CHECK_FUNC(inet_pton,,
                        [AC_CHECK_LIB(resolv,inet_pton,LIBS="-lresolv $LIBS")])
                AC_CHECK_FUNC(pow,,
                        [AC_CHECK_LIB(m,pow,LIBS="-lm $LIBS")])
    ;;
    *)
    ;;
esac
AC_DEFINE_UNQUOTED(NULL_DEVICE, "$null_device",
         [Your system's bitbucket (usually /dev/null or NUL)])
AC_SUBST(PIC_LIBS)

dnl ******************************
dnl Check if this was CVS source
dnl ******************************
if test -f "${srcdir}/README.cvs" ; then
       PL_FROM_CVS=yes
fi

### --------------------------------------------------------------------------
### popt

AC_CHECK_LIB(popt, poptStrippedArgv,, [AC_MSG_ERROR([

  popt 1.5 or newer is required to build cashutil. You can download
  the latest version from ftp://people.redhat.com/sopwith/popt/, or if
  you're running Debian, install the libpopt-dev package.
])])

dnl ******************************
dnl readline checking
dnl ******************************
VL_LIB_READLINE

dnl ******************************
dnl Cashutil Checks
dnl ******************************
AC_HEADER_STDC
AC_C_CONST
AC_HEADER_TIME
AC_TYPE_SIZE_T
AC_C_INLINE
AC_HEADER_TIME
AC_STRUCT_TM
AC_PROG_GCC_TRADITIONAL
AC_TYPE_SIGNAL
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_CHOWN
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_FORK
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_FUNC_VPRINTF
AC_HEADER_DIRENT
AC_TYPE_PID_T

AC_CHECK_HEADERS(
	dirent.h errno.h fcntl.h inttypes.h memory.h netdb.h 	  \
	netinet/in.h regex.h stdint.h stdlib.h string.h strings.h \
	sys/ioctl_compat.h sys/ioctl.h 	sys/malloc.h sys/select.h \
	sys/sockio.h sys/time.h sys/utsname.h unistd.h popt.h     \
	ifaddrs.h inttypes.h langinfo.h libintl.h limits.h )

AC_CHECK_FUNCS(
	malloc memcpy memmove memset mkdir setenv putenv \
	sigaction snprintf strchr strdup getline stpcpy  \
	strtoul strerror getcwd gettimeofday localtime_r \
	regcomp setlocale strcasecmp strcspn strrchr \
	strspn strstr strtol strtok)

dnl *************************************
dnl QOF
dnl *************************************
AC_DEFUN([AS_SCRUB_INCLUDE],
[
  GIVEN_CFLAGS=$[$1]
  INCLUDE_DIRS=`echo | cpp -v 2>&1`

  dnl remove everything from this output between the "starts here" and "End of"
  dnl line
  INCLUDE_DIRS=`echo $INCLUDE_DIRS | sed -e 's/.*<...> search starts here://' | sed -e 's/End of search list.*//'`
  for dir in $INCLUDE_DIRS; do
    GIVEN_CFLAGS=$(echo $GIVEN_CFLAGS | sed -e "s;-I$dir ;;" | sed -e "s;-I$dir$;;")
  done
  [$1]=$GIVEN_CFLAGS
])

AC_ARG_WITH(qof, [  --with-qof=path         prefix for Query Object Framework - QOF (auto)])
QOF_REQUIRED=0.6.0
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test pkg-config = no; then
	AC_MSG_ERROR([Please install pkgconfig])
	exit 1
fi
AC_MSG_CHECKING([for QOF, version >= $QOF_REQUIRED])
if test "$withval" != "yes"; then
	QOF=`$PKG_CONFIG --exists '$withval/lib/pkgconfig/qof-1.pc >= $QOF_REQUIRED'`
	QOF_LIBS=`$PKG_CONFIG --libs $withval/lib/pkgconfig/qof-1.pc`
	QOF_CFLAGS=`$PKG_CONFIG --cflags $withval/lib/pkgconfig/qof-1.pc`
	QOF_VERSION=`$PKG_CONFIG --modversion $withval/lib/pkgconfig/qof-1.pc`
	QOF_PREFIX=`$PKG_CONFIG --variable=prefix $withval/lib/pkgconfig/qof-1.pc`
	QOF_LIB_DIR=`$PKG_CONFIG --variable=libdir $withval/lib/pkgconfig/qof-1.pc`
	QOF_XML_DIR=`$PKG_CONFIG --variable=xmldir $withval/lib/pkgconfig/qof-1.pc`
else
	QOF=`$PKG_CONFIG --exists 'qof-1 >= $QOF_REQUIRED'`
	QOF_LIBS=`$PKG_CONFIG --libs qof-1`
	QOF_CFLAGS=`$PKG_CONFIG --cflags qof-1`
	QOF_VERSION=`$PKG_CONFIG --modversion qof-1`
	QOF_PREFIX=`$PKG_CONFIG --variable=prefix qof-1`
	QOF_LIB_DIR=`$PKG_CONFIG --variable=libdir qof-1`
	QOF_XML_DIR=`$PKG_CONFIG --variable=xmldir qof-1`
fi
AC_SUBST(QOF_CFLAGS)
AC_SUBST(QOF_LIBS)
AS_SCRUB_INCLUDE(QOF_PREFIX)
AC_SUBST(QOF_PREFIX)
AC_SUBST(QOF_LIB_DIR)
AC_SUBST(QOF_XML_DIR)
if test x$QOF_XML_DIR = x; then
	AC_MSG_RESULT([no])
	AC_MSG_ERROR([
	CashUtil requires the Query Object Framework: QOF.
	You need to install libqof1 >= 0.6.0
	You can find it at http://qof.sourceforge.net/
])
	exit 1
else
	AC_MSG_RESULT([yes])
fi

dnl *************************************
dnl GLib
dnl *************************************

AC_PATH_PROG(PKG_CONFIG,pkg-config)
AM_PATH_GLIB_2_0("2.0.0", , ,gobject)
if test "x$PKG_CONFIG" != x; then
	GLIB_LIBS=`$PKG_CONFIG --libs glib-2.0`
	GLIB_CFLAGS=`$PKG_CONFIG --cflags glib-2.0`
	GOBJECT_LIBS=`$PKG_CONFIG --libs gobject-2.0`
	GMODULE_LIBS=`$PKG_CONFIG --libs gmodule-2.0`
fi
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)
AC_SUBST(GOBJECT_LIBS)
AC_SUBST(GMODULE_LIBS)

dnl *************************************
dnl Check for xsltproc
dnl *************************************
AM_CONDITIONAL(BUILD_XML,[test ! -f "${srcdir}/doc/man/cashutil.1"])
if test ! -f "${srcdir}/doc/man/cashutil.1" ; then

# It's just rude to go over the net to build
XSLTPROC_FLAGS=--nonet
DOCBOOK_ROOT=
XSLTROOT="../../doc/xml"
case "$host" in
        *darwin*)
	DOCBOOK_ROOT="/sw/share/xml/xsl/docbook-xsl"
	XSLTROOT="/sw/share/xml/xsl/docbook-xsl/manpages/"
	XSLTPROC_FLAGS="$XSLTPROC_FLAGS --novalid"
	;;
esac
AC_SUBST(XSLTROOT)
if test ! -f /etc/xml/catalog; then
for i in /usr/share/sgml/docbook/stylesheet/xsl/nwalsh /usr/share/docbook2X/xslt/man/ /usr/share/xml/docbook/stylesheet/nwalsh/manpages/ /sw/share/xml/xsl/docbook-xsl;
	do
		if test -d "$i"; then
		DOCBOOK_ROOT=$i
        fi
	done
# Last resort - try net
if test -z "$DOCBOOK_ROOT"; then
   XSLTPROC_FLAGS=
fi
else
   XML_CATALOG=/etc/xml/catalog
   CAT_ENTRY_START='<!--'
   CAT_ENTRY_END='-->'
fi

AC_CHECK_PROG(XSLTPROC,xsltproc,xsltproc,)
XSLTPROC_WORKS=no
if test -n "$XSLTPROC"; then
    AC_MSG_CHECKING([whether xsltproc works])

    if test -n "$XML_CATALOG"; then
       DB_FILE="http://docbook.sourceforge.net/release/xsl/current/xhtml/docbook.xsl"
    else
       DB_FILE="$XSLTROOT/docbook.xsl"
    fi
   $XSLTPROC $XSLTPROC_FLAGS $DB_FILE << END
<?xml version="1.0" encoding='ISO-8859-1'?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.1.2//EN"
"http://www.oasis-open.org/docbook/xml/4.1.2/docbookx.dtd">
<book id="test">
</book>
END
        if test "$?" = 0; then
           XSLTPROC_WORKS=yes
        fi
        AC_MSG_RESULT($XSLTPROC_WORKS)
fi
fi
AM_CONDITIONAL(have_xsltproc, test "$XSLTPROC_WORKS" = "yes")

AC_SUBST(XML_CATALOG)
AC_SUBST(XSLTPROC_FLAGS)
AC_SUBST(DOCBOOK_ROOT)
AC_SUBST(CAT_ENTRY_START)
AC_SUBST(CAT_ENTRY_END)
AC_SUBST(XSLTPROC)

### --------------------------------------------------------------------------
### Check for perl

# Check for perl, force version 5
AC_ARG_WITH(perl,
  [  --with-perl=FILE        which perl executable to use ],
  PERL="${with_perl}")

# If the user didn't specify a perl, then go fetch.
if test x"$PERL" = x;
then
  AC_PATH_PROG(PERL, perl)
fi

# Make sure Perl was found
if test x"$PERL" = x; then
    AC_MSG_ERROR([Cannot find Perl. Try using the --with-perl flag.])
fi

# Make sure it's version 5 or later
if "$PERL" -e 'exit 1 if $] < 5.0'; then
        :
else
    AC_MSG_ERROR([Found ${PERL} reports version ]
                 [`${PERL} -e 'print $]'`, need version 5.*])
fi
AC_SUBST(PERL)

dnl *******************************
dnl Internationalization
dnl *******************************

ALL_LINGUAS="en_GB "
GETTEXT_PACKAGE=cashutil
AC_SUBST(GETTEXT_PACKAGE)
AM_GLIB_GNU_GETTEXT
AC_SUBST(INTLTOOL_XGETTEXT)
AC_DEFINE_UNQUOTED([GETTEXT_PACKAGE], ["${GETTEXT_PACKAGE}"], [gettext domain])

dnl *************************************
dnl Locate the backend library 
dnl *************************************

GNC_LIB_DIR=`eval echo $libdir | sed "s%^NONE%$prefix%"`
GNC_LIB_DIR=`eval echo $GNC_LIB_DIR | sed "s%^NONE%$prefix%"`
AC_SUBST(GNC_LIB_DIR)

dnl ******************************
dnl Defaults for GCC
dnl ******************************
AC_MSG_CHECKING(what extra warning flags to pass to the C compiler)
if test ${GCC}x = yesx; then
  warnFLAGS=
  CFLAGS="${CFLAGS} -g2 -Wall"
  AC_ARG_ENABLE(error-on-warning,
        [  --disable-error-on-warning    disable treating compile warnings as errors],
        [case "${enableval}" in
        yes) warnFLAGS="${warnFLAGS} -Werror" ;;
        no)  ;;
        *) AC_MSG_ERROR(bad value ${enableval} for --enable-error-on-warning) ;;
        esac],
        [  warnFLAGS="${warnFLAGS} -Werror" ])
  GCC_VERSION=`${CC} -dumpversion`
  if test `echo ${GCC_VERSION} | cut -d. -f1` -ge 3; then
     # This is gcc >= 3.x.x
     if test `echo ${GCC_VERSION} | cut -d. -f2` -ge 4; then
        # This is gcc >= 3.4.x
        warnFLAGS="${warnFLAGS} -Wdeclaration-after-statement"
     fi
  fi
  CFLAGS="${CFLAGS} ${warnFLAGS}"
else
  warnFLAGS=
fi
AC_MSG_RESULT($warnFLAGS)

case "$host" in
	*bsd*)
		AC_DEFINE(TTYPrompt, "/dev/cua[<0..n>]", [Define verbose tty device])
	;;
	*)
		AC_DEFINE(TTYPrompt, "/dev/tty[<0..n>]", [Define verbose tty device])
	;;
esac

AC_OUTPUT([ po/Makefile.in
  Makefile
  src/Makefile
  src/cashutil.h
  doc/Makefile
  doc/doxygen.cfg
  doc/man/Makefile
  doc/xml/Makefile
  doc/xml/catalog.xml
  doc/xml/docbook.xsl
])

AC_MSG_RESULT([
  Options detected/selected
  -------------------------
  cashutil version ..... : $VERSION
  Build for host ....... : $host
  Extra Warnings ....... : $warnFLAGS
  CPPFLAGS ............. : $CPPFLAGS
  CFLAGS ............... : $CFLAGS
  LDFLAGS .............. : $LDFLAGS
  xsltproc flags ........: $XSLTPROC_FLAGS
  QOF support ...........: $QOF_VERSION
  QOF location ..........: $QOF_PREFIX
  QOF library dir .......: $QOF_LIB_DIR
  QOF backend config ....: $QOF_XML_DIR
  GNC Backend location ..: $GNC_LIB_DIR
])

if [[ x"$PL_FROM_CVS" = xyes ]]; then
  echo " .----- NOTICE ------------------------------------------------."
  echo " |         You are using Cashutil from CVS source.             |"
  echo " |                                                             |"
  echo " | This is likely to be unstable, or contain some incomplete   |"
  echo " | features, or just plain not work at all. Use it at your own |"
  echo " | risk. Please help me to fix any bugs you find, by reporting |"
  echo " | them back to me via the QOF-devel mailing list.             |"
  echo " | http://lists.sourceforge.net/lists/listinfo/qof-devel       |"
  echo " \`-------------------------------------------------------------'"
  echo ""
fi
