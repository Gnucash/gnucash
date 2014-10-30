## -*-m4-*-

dnl Process this file with autoconf to produce a configure script.

# FILE:
# configure.in
#
# FUNCTION:
# implements checks for a variety of system-specific functions
#
# Contents:
#   Headers   - Autoconf header stuff
#   Variables - Hardcoded variables
#   Programs  - Check for programs binaries
#   Functions - Check for functions
#   With      - Check for --with/without options
#   Enable    - Check for --enable/disable options
#   Libraries - Check for libraries
#   Footer    - Autoconf footer stuff

# Autoconf initialization
AC_PREREQ(2.60)
AC_INIT([GnuCash], [2.6.99], [gnucash-devel@gnucash.org])
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_SRCDIR(src/engine/Transaction.h)
AC_CONFIG_MACRO_DIR([macros])

#Change this in development versions when changing anything that
#affects stored data structures. Reset to zero when bumping version.
GNUCASH_NANO_VERSION=0
#This should be the earliest version in the form XXMMUUNN (XX=MAJOR,
#MM=MINOR, UU=MICRO, NN=NANO) or SVN revision if before 2.4.1 which is
#storage-compatible with the current version. See the comments in
#src/backend/dbi/gnc-backend-dbi.c:gnc_dbi_load.
GNUCASH_RESAVE_VERSION=19920
# Initialize automake -- make sure we have at least version 1.9 Note:
# Automake 1.14 issues a ton of warnings about subdir-objects, which
# will become the default in automake 2.0; unfortunately this option
# is broken and prevents building without passing
# --disable-dependency-tracking to configure, which make-distcheck
# doesn't do. See
# http://lists.gnu.org/archive/html/automake/2014-04/msg00002.html and
# http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13928 for details.
# In the meantime the best option is to use Automake 1.13 or earlier.
AM_INIT_AUTOMAKE([1.9 dist-bzip2])

# Parse out the version number
GNUCASH_VERSION_STRING=AC_PACKAGE_VERSION
GNUCASH_MAJOR_VERSION=`echo $GNUCASH_VERSION_STRING | [ sed -e 's/^\([^\.]*\)\.\([^.]*\)\.\(.*\)$/\1/' ]`
GNUCASH_MINOR_VERSION=`echo $GNUCASH_VERSION_STRING | [ sed -e 's/^\([^\.]*\)\.\([^.]*\)\.\(.*\)$/\2/' ]`
GNUCASH_MICRO_VERSION=`echo $GNUCASH_VERSION_STRING | [ sed -e 's/^\([^\.]*\)\.\([^.]*\)\.\(.*\)$/\3/' ]`
GNUCASH_LATEST_STABLE_SERIES=$GNUCASH_MAJOR_VERSION.$(($GNUCASH_MINOR_VERSION-($GNUCASH_MINOR_VERSION%2)))

# save any user/environment optimization flags before we muck with them in
# --enable-debug:
USER_OPTIMIZATION=""
for flag in $CFLAGS; do
  tmp_flag=`echo $flag | sed -e 's,-O.,,'`
  if test -z "$tmp_flag"; then
    USER_OPTIMIZATION="$USER_OPTIMIZATION ${flag}"
  fi
done

##  Do this first, because the other tests depend on it:

# Check for gcc and intltool (somehow breaks the suggested content
# order, doesn't it? Whatever.)
AC_PROG_CC
AM_PROG_CC_C_O
AC_GNU_SOURCE
AC_PROG_INTLTOOL
# Ensure the compiler supports C++ 11:
AX_CXX_COMPILE_STDCXX_11(ext)


AC_SUBST(GNUCASH_MAJOR_VERSION)
AC_SUBST(GNUCASH_MINOR_VERSION)
AC_SUBST(GNUCASH_MICRO_VERSION)
AC_SUBST(GNUCASH_NANO_VERSION)
AC_SUBST(GNUCASH_LATEST_STABLE_SERIES)

AC_DEFINE_UNQUOTED(GNUCASH_MAJOR_VERSION, $GNUCASH_MAJOR_VERSION,
    [GnuCash Major version number])
AC_DEFINE_UNQUOTED(GNUCASH_MINOR_VERSION, $GNUCASH_MINOR_VERSION,
    [GnuCash Minor version number])
AC_DEFINE_UNQUOTED(GNUCASH_MICRO_VERSION, $GNUCASH_MICRO_VERSION,
    [GnuCash Micro version number])
AC_DEFINE_UNQUOTED(GNUCASH_NANO_VERSION, $GNUCASH_NANO_VERSION,
    [GnuCash Nano version number])
AC_DEFINE_UNQUOTED(GNUCASH_RESAVE_VERSION, $GNUCASH_RESAVE_VERSION,
    [GnuCash earliest compatible databaseversion number])
AC_DEFINE_UNQUOTED(GNUCASH_LATEST_STABLE_SERIES, "$GNUCASH_LATEST_STABLE_SERIES",
    [Most recent stable GnuCash series])

dnl Set of available languages.
ALL_LINGUAS="ar bg brx ca cs da de doi el en_GB es_NI es eu fa fi fr he hi hu it ja ko kok ks lt lv mr nb ne nl pl pt_BR pt ro ru rw sk sv ta te tr uk ur vi zh_CN zh_TW"

GETTEXT_PACKAGE=gnucash
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE",
    [GetText version number])

AM_GLIB_GNU_GETTEXT


# Enable only when we have autoconf --version >= 2.59
# on all platforms. Note that AC_PREREQ(2.59) may be
# insufficient to actually use v2.59 on OSX
#AC_CONFIG_MACRO_DIR(macros)

AC_PROG_INSTALL
PKG_PROG_PKG_CONFIG

dnl clang wants to check file encodings, and we need to tell it not to
dnl becuase of the unicode currency symbols. But older versions of GCC
dnl don't like the warning suppression option, so we need to set it only
dnl with clang.

AC_MSG_CHECKING([if compiling with clang])

AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM([], [[
#ifndef __clang__
       not clang
#endif
]])],
[ac_cv_CLANG=yes], [ac_cv_CLANG=no])

AC_MSG_RESULT([$ac_cv_CLANG])
AM_CONDITIONAL([CLANG], [test "x$ac_cv_CLANG" = "xyes"])

###--------------------------------------------------------
### Begin C++ dependencies
###--------------------------------------------------------
AX_BOOST_BASE([1.48.0], [ac_cv_BOOST=yes], [ac_cv_BOOST=no])
if test x$ac_cv_BOOST != xyes; then
    AC_MSG_ERROR([Boost 1.48.0 or later was not found and is required to build GnuCash])
fi
###--------------------------------------------------------
### Optionally enable gtkmm plugin (requires c++ compiler)
###--------------------------------------------------------
enable_GTKMM=false

AC_ARG_ENABLE(gtkmm,
  [AS_HELP_STRING([--enable-gtkmm],[enable gtkmm gui])],
  [case "${enableval}" in
     yes) enable_GTKMM=true ;;
     no) enable_GTKMM=false ;;
     *) enable_GTKMM=false ;;
   esac]
  )

AS_IF([test x${enable_GTKMM} = "xtrue"],
  [
    # We require gtkmm, of course
    PKG_CHECK_MODULES(GTKMM, gtkmm-2.4 >= 2.24)
    # and also the c++ compiler
    AC_PROG_CXXCPP
    AC_PROG_CXX
    _COMPONENTS="$_COMPONENTS gtkmm"
  ])
AM_CONDITIONAL([WITH_GTKMM], [test x${enable_GTKMM} = "xtrue"])
# This section must come before AC_PROG_LIBTOOL because libtool
# must know about the c++ compiler.
###--------------------------------------------------------


AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AM_DISABLE_STATIC
AM_PROG_LIBTOOL

AC_SUBST(LIBTOOL_DEPS)

AC_ISC_POSIX
AC_C_BIGENDIAN
AC_PROG_MAKE_SET
AC_PROG_LN_S
AC_HEADER_STDC

AC_CHECK_HEADERS(limits.h sys/time.h sys/times.h sys/wait.h pow.h)
AC_CHECK_FUNCS(stpcpy memcpy timegm towupper)
AC_CHECK_FUNCS(setenv,,[
  AC_CHECK_FUNCS(putenv,,[
    AC_MSG_ERROR([Must have one of the setenv or putenv functions.])
  ])
])

STRUCT_TM_GMTOFF_CHECK
cflags_save=${CFLAGS}
CFLAGS="${CFLAGS} -Wall -Werror"
SCANF_LLD_CHECK
if test x$am_cv_scanf_lld = "xno"; then
  SCANF_QD_CHECK

  if test x$am_cv_scanf_qd = "xno"; then
    SCANF_I64D_CHECK
    if test x$am_cv_scanf_i64d = "xno"; then
      AC_MSG_ERROR([Cannot continue, no long long conversion support in scanf.])
    fi
  fi
fi
CFLAGS="${cflags_save}"

# test whether we are building directly from svn/svk/git/bzr
AC_MSG_CHECKING(if building from an scm managed directory)
"$srcdir/util/gnc-scm-info" -r "$srcdir" >/dev/null 2>&1
if test $? = 0 -o "x${BUILDING_FROM_SCM}" = "xyes" ; then
  BUILDING_FROM_SCM=yes
  SCM_TYPE=$("$srcdir/util/gnc-scm-info" -t "$srcdir")
  AC_MSG_RESULT($SCM_TYPE)

  if test "x${SCM_TYPE}" = "xgit"; then
    AC_MSG_CHECKING(git command to use due to detected git repo)
    # The windows build uses environment variable $GIT_CMD to invoke git (adding git
    # to the PATH in Windows results in a build failure).
    # So for platform independence, use GIT_CMD for all
    if test -z "${GIT_CMD}" ; then
      GIT_CMD=git
    fi
    AC_MSG_RESULT(${GIT_CMD})
  fi
else
  AC_MSG_RESULT(no)
  BUILDING_FROM_SCM=no
  SCM_TYPE=

  # Make sure we've got swig-runtime.h and gnc-scm-info.h
  AC_CHECK_FILE(${srcdir}/src/swig-runtime.h, [],
    [AC_MSG_ERROR([

It looks like you are NOT building from Subversion, svk, git or bzr
but I cannot find swig-runtime.h.  Check your PATH and make sure
we can find gnc-scm-info in your PATH!
Either that or contact gnucash-devel@gnucash.org because
the tarball you downloaded is broken.

    ])])
  AC_CHECK_FILE(${srcdir}/src/core-utils/gnc-scm-info.h, [],
    [AC_MSG_ERROR([

It looks like you are NOT building from Subversion, svk, git or bzr
but I cannot find gnc-scm-info.h.  Check your PATH and make sure
we can find gnc-scm-info in your PATH!
Either that or contact gnucash-devel@gnucash.org because
the tarball you downloaded is broken.

    ])])
fi
AM_CONDITIONAL(BUILDING_FROM_SCM, test "x$BUILDING_FROM_SCM" = "xyes")
AC_SUBST(SCM_TYPE)
AC_SUBST(GIT_CMD)

# Build dir adjustments
AM_CONDITIONAL(GNUCASH_SEPARATE_BUILDDIR, test "x${srcdir}" != "x.")

# Init binreloc
AM_BINRELOC

### --------------------------------------------------------------------------
### See if we have pkg-config

AC_PATH_PROG(PKG_CONFIG,pkg-config)
if test "x$PKG_CONFIG" = x; then
  # @*%&$ Ximian programmers renamed this application
  AC_PATH_PROG(PKG_CONFIG,pkgconfig)
  if test "x$PKG_CONFIG" = x; then
    AC_MSG_ERROR([Could not find the pkg-config (or pkgconfig) program.],)
  fi
fi

### --------------------------------------------------------------------------
### Glib checks.

# We require glib >= 2.28, released together with gtk-2.24;
# We first check for other versions due to deprecations.
# 2.36 deprecated g_type_init
# 2.32 deprecated some gthread functions
PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.38 gio-2.0 >= 2.25 gthread-2.0 gobject-2.0 gmodule-2.0,
[
  AC_DEFINE([HAVE_GLIB_2_38], [1], [Configure leading quote around assertions])
  AC_DEFINE([HAVE_GLIB_2_36], [1], [Configure g_type_init deprecation])
  AC_DEFINE([HAVE_GLIB_2_32], [1], [Configure gthread deprecations])
],
[
  PKG_CHECK_MODULES(GLIB,
                        glib-2.0 >= 2.36 gio-2.0 >= 2.25 gthread-2.0 gobject-2.0 gmodule-2.0,
  [
    AC_DEFINE([HAVE_GLIB_2_36], [1], [Configure g_type_init deprecation])
    AC_DEFINE([HAVE_GLIB_2_32], [1], [Configure gthread deprecations])
  ],
  [
    PKG_CHECK_MODULES(GLIB,
                        glib-2.0 >= 2.32 gio-2.0 >= 2.25 gthread-2.0 gobject-2.0 gmodule-2.0,
    [
        AC_DEFINE([HAVE_GLIB_2_32], [1], [Configure gthread deprecations])
    ],
    [
	PKG_CHECK_MODULES(GLIB,
                        glib-2.0 >= 2.28 gio-2.0 >= 2.25 gthread-2.0 gobject-2.0 gmodule-2.0)
    ])
  ])
])
AC_CHECK_HEADERS(dirent.h dlfcn.h dl.h utmp.h locale.h mcheck.h unistd.h wctype.h)

GLIB_GSETTINGS

# I'm sure we (used to?) require this in various places, so don't remove
# this test unless you've done sufficient code review/testing.
AC_MSG_CHECKING(if unsigned long is at least as big as guint32)
GNC_OLDCFLAGS="$CFLAGS"
GNC_OLDLDFLAGS="$LDFLAGS"
CFLAGS="${CFLAGS} ${GLIB_CFLAGS}"
LDFLAGS="${LDFLAGS} ${GLIB_LIBS}"
AC_TRY_RUN([
  #include <glib.h>
  int main(int argc, char *argv[]) {
    return(!(sizeof(unsigned long) >= sizeof(guint32)));
  }
],[
  AC_MSG_RESULT(yes)
],[
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(cannot continue, size of unsigned long too small.)
],[
  AC_MSG_RESULT(assuming yes)
])
CFLAGS="$GNC_OLDCFLAGS"
LDFLAGS="$GNC_OLDLDFLAGS"

# Gnucash replaced dlopen/dlsym by the g_module functions; dlsym
# is needed optionally in one place for BSD linkers, though.
DL_LIB=
AC_CHECK_FUNCS(dlsym,,[
  AC_CHECK_LIB(dl, dlsym, DL_LIB="-ldl",[
    AC_CHECK_LIB(dld, shl_load, DL_LIB="-ldld",[
      AC_CHECK_FUNCS(dlsym, DL_LIB="")
    ])
  ])
])

oLIBS="$LIBS"
LIBS="$LIBS $DL_LIB"
AC_CHECK_FUNCS(dlerror,,)
LIBS="$oLIBS"
AC_SUBST(DL_LIB)

##################################################
# OS dependent checks
##################################################
AC_SUBST(SED)

# Set some defaults, they may be overridden below
REGEX_LIBS=""
GCC_VERSION=`${CC} -dumpversion`

AC_MSG_CHECKING(operating system)
#NOUNDEF for use with mingw
NOUNDEF=""
case $host_os in
  rhapsody* | darwin[1567].*)
    AC_MSG_RESULT([darwin, but too old])
    AC_MSG_ERROR([This platform is not supported, please
update to latest  darwin])
    ;;
  darwin*)
    platform=darwin
    AC_DEFINE(PLATFORM_OSX,1,[Running on OSX, either X11 or Quartz])
    AC_MSG_RESULT($platform)
    AC_MSG_CHECKING(For GDK-Quartz)
    platform=osx
    _gdk_tgt=`$PKG_CONFIG --variable=target gdk-2.0`
    if test "x$_gdk_tgt" = xquartz; then
      platform=darwin/quartz
      AC_MSG_RESULT(yes)
      AC_DEFINE(GDK_QUARTZ,,[Using GDK Quartz (not X11)])
      PKG_CHECK_MODULES([GTK_MAC], gtk-mac-integration,
                        [_gtk_mac=yes], [_gtk_mac=no])
      if test "x$_gtk_mac" = xyes; then
        GTK_MAC_LIBS="${GTK_MAC_LIBS} -lobjc -framework Cocoa"
        GTK_MAC_CFLAGS="${GTK_MAC_CFLAGS} -xobjective-c"
 	if test "`echo ${GCC_VERSION} | cut -d. -f1`" -eq 4; then
	   if test "`echo ${GCC_VERSION} | cut -d. -f2`" -ge 8; then
      	   # This is gcc >= 4.8.x
	      GTK_MAC_CFLAGS="${GTK_MAC_CFLAGS} -fobjc-exceptions"
	   fi
	fi
     fi
      AC_MSG_RESULT(${_gtk_mac})
      AC_SUBST(GTK_MAC_LIBS)
      AC_SUBST(GTK_MAC_CFLAGS)
    else
      AC_MSG_RESULT(no)
    fi
    ;;
  mingw*|cygwin*)
    platform=win32
    AC_MSG_RESULT($platform)
    LT_PROG_RC

    AC_MSG_CHECKING(for native win32)
    case $host_os in
      mingw*)
        native_win32=yes
        AC_MSG_RESULT(yes)

        HTMLHELP_LIBS=
        AC_MSG_CHECKING(for HtmlHelpW)
        saved_LIBS="${LIBS}"
        LIBS="${LIBS} -lhtmlhelp"
        AC_LINK_IFELSE(
          [AC_LANG_PROGRAM(
             [[#include <windows.h>
               #include <htmlhelp.h>]],
             [HtmlHelpW(0, (wchar_t*)"", HH_HELP_CONTEXT, 0);])],
          [AC_MSG_RESULT(yes)
           HTMLHELP_LIBS=-lhtmlhelp
           AC_DEFINE(HAVE_HTMLHELPW,1,[System has HtmlHelpW])
          ], [AC_MSG_RESULT(no)])
        LIBS="${saved_LIBS}"
        AC_SUBST(HTMLHELP_LIBS)
        ;;
      *)
        AC_MSG_RESULT(no)
        ;;
    esac

    REGEX_LIBS="-lregex"

    # Adjust CFLAGS and LDFLAGS on Windows
    CFLAGS="${CFLAGS} -mms-bitfields"
#   NOUNDEF will be added to ldflags for the makefiles but not the configures
#   This fixes a problem with libtool and gcc 4.8
    NOUNDEF="-no-undefined"
    LDFLAGS="${LDFLAGS} -mwindows"
    ;;

  *)
    platform=linux/other
    AC_MSG_RESULT($platform)
    ;;
esac
AC_SUBST(REGEX_LIBS)

AM_CONDITIONAL(PLATFORM_LINUX, test "x$platform" = "xlinux/other")
AM_CONDITIONAL(PLATFORM_OSX, test "x$platform" = "xdarwin")
AM_CONDITIONAL(PLATFORM_OSX_QUARTZ, test "x$platform" = "xdarwin/quartz")
AM_CONDITIONAL(PLATFORM_WIN32, test "x$platform" = "xwin32")
AM_CONDITIONAL(OS_WIN32, test "x$native_win32" = "xyes")

# These are unavailable on windows/mingw32 and X11 isn't desired or
# required for MacOSX Quartz
if test "x$_gdk_tgt" = xquartz;
then
    AC_CHECK_HEADERS(glob.h)
else
    AC_CHECK_HEADERS(X11/Xlib.h glob.h)
fi
AM_CONDITIONAL(HAVE_X11_XLIB_H, test "x$ac_cv_header_X11_Xlib_h" = "xyes")
AC_CHECK_FUNCS(chown gethostname getppid getuid gettimeofday gmtime_r)
AC_CHECK_FUNCS(gethostid link)
##################################################



### --------------------------------------------------------------------------
### Guile version checks

# - check minimum version
# - determine GUILE_CFLAGS and GUILE_LIBS
# Note: systems that install both guile 1.8 and guile 2 use different
#       names for the 1.8 and 2.0 autoconf macros to avoid conflicts.
#       So the tests below will check for known macro names in the order
#       that we assume will always result in the proper macros being called
#       for the guile version that has been detected.
gnc_have_guile_2=no
gnc_have_guile_www=no
PKG_CHECK_MODULES(GUILE,
                  [guile-1.8 >= 1.8.5],
                  [m4_ifdef([GUILE1_8_PROGS],
                            [ GUILE1_8_PROGS ],
                            [ m4_ifdef([GUILE1_PROGS],
                                       [ GUILE1_PROGS ],
                                       [ GUILE_PROGS ])])
                  ], [
  PKG_CHECK_MODULES(GUILE,
                  [guile-2.0 >= 2.0.0],
                  [m4_ifdef([GUILE_PROGS],
                            [ GUILE_PROGS
                              gnc_have_guile_2=yes ],
                            [ AC_MSG_ERROR([ guile 2 is found on your system, but appears
    to export different autoconf macros than what we expected. Please report this as a bug
    in GnuCash, so we can fix this for your platform.]) ])
                  ],
                  [AC_MSG_ERROR([
    guile does not appear to be installed correctly, or is not in the
    correct version range.  Perhaps you have not installed the guile
    development packages?  Gnucash requires at least version 1.8.5 to build.
  ])])
])

AM_CONDITIONAL(GNC_HAVE_GUILE_2, test "${gnc_have_guile_2}" = yes)


### --------------------------------------------------------------------------
### SWIG version checks (only when building from SCM)
#
# In order to build against guile 2 or build the dist we need at least
# version 2.0.10 of SWIG, because that's the first version that supports
# guile 2.
#
# For a guile 1.8 build we accept an older version, but wont allow you
# to build the dist because we need to apply a patch to the swig
# generated files that needs to go into the release tarball. At that
# point we don't know yet which version of guile will be used together
# with that tarball so we have to prepare the tarball for guile 2
# compatibility.
#
if test "${BUILDING_FROM_SCM}" = yes
then
    AX_PKG_SWIG(2.0.10, [gnc_have_swig_2_0_10=yes], [gnc_have_swig_2_0_10=no])

    if test "${gnc_have_guile_2}" = yes
    then
        if test "${gnc_have_swig_2_0_10}" = no
        then
            AC_MSG_ERROR([
    You are building from ${SCM_TYPE} but swig was not found or too old.
    To build gnucash you need at least swig version 2.0.10.
            ])
        fi
    else  # using guile 1.8
        if test "${gnc_have_swig_2_0_10}" = no
        then
            AX_PKG_SWIG(1.3.31, [],
                [AC_MSG_ERROR([
    You are building from ${SCM_TYPE} but swig was not found or too old.
    To build gnucash with guile 1.8 you need at least swig version 1.3.31.
            ])])
        fi
    fi

    # If we get here then we have an okay version to build locally.
    # Check if we can build dist (which always requires swig 2.0.10)
    if test "${gnc_have_swig_2_0_10}" = no
    then
        AC_MSG_WARN([You don't have SWIG 2.0.10 so you will not be able to "make dist"])
    fi
    AM_CONDITIONAL([SWIG_DIST_FAIL], test "${gnc_have_swig_2_0_10}" != yes)
else
    # When building from tarball, all the prerequisites to build dist are already
    # met so make sure it's allowed
    AM_CONDITIONAL([SWIG_DIST_FAIL], test yes != yes)
fi

### --------------------------------------------------------------------------
### LIBXML -- GNOME_XML_LIB is defined by GNOME_XML_CHECK

LIBXML2_REQUIRED=2.5.10
PKG_CHECK_MODULES(LIBXML2, libxml-2.0 >= $LIBXML2_REQUIRED)
AS_SCRUB_INCLUDE(LIBXML2_CFLAGS)
AC_SUBST(LIBXML2_CFLAGS)
AC_SUBST(LIBXML2_LIBS)

oLIBS="$LIBS"
LIBS="$LIBS $LIBXML2_LIBS"

# Compile a test program to verify that the xml2 library works.
AC_CHECK_LIB(xml2, xmlElemDump, [:], [
  AC_MSG_ERROR([libxml2 is required for GnuCash])
  ])
LIBS="$oLIBS"

### --------------------------------------------------------------------------
### LIBXSLT

PKG_CHECK_MODULES(LIBXSLT, libxslt)
AS_SCRUB_INCLUDE(LIBXLT_CFLAGS)
AC_SUBST(LIBXSLT_CFLAGS)
AC_SUBST(LIBXSLT_LIBS)

# xsltproc is not included in libxslt on all distributions
# so check for it separately
AC_PATH_PROG(XSLTPROC, xsltproc, [""])

if test -z "$XSLTPROC"; then
  AC_MSG_ERROR([xsltproc is required to build GnuCash])
fi

### --------------------------------------------------------------------------
### Zlib

ZLIB_LIBS=-lz
AC_SUBST(ZLIB_LIBS)

oLIBS="$LIBS"
LIBS="$LIBS $ZLIB_LIBS"
AC_CHECK_LIB(z, gzopen, [:], [
  AC_MSG_ERROR([zlib is required for GnuCash])
])
LIBS="$oLIBS"

### --------------------------------------------------------------------------
### Internal code part which is called "qof"

LIBQOF_LIBRARY_VERSION=1:4:0
AC_SUBST(LIBQOF_LIBRARY_VERSION)

QOF_CFLAGS='-I${top_builddir}/src/libqof/qof -I${top_srcdir}/src/libqof/qof'
QOF_LIBS='${top_builddir}/src/libqof/qof/libgnc-qof.la'
AC_SUBST(QOF_CFLAGS)
AC_SUBST(QOF_LIBS)

### ----------------------
### LIBDBI

AC_ARG_ENABLE(dbi,
  [AS_HELP_STRING([--disable-dbi],[don't build with the libdbi backend])],
  [case "${enableval}" in
     yes) want_dbi=true ;;
     no)  want_dbi=false ;;
     *) want_dbi=false ;;
   esac],
   [want_dbi=true])
if test x${want_dbi} = xtrue
then
  EXTRA_SEARCH_LIBS=""
  AC_CHECK_HEADERS(dbi/dbi.h)
  if test "x$ac_cv_header_dbi_dbi_h" != xno; then
    save_libs=$LIBS
    AC_ARG_WITH( dbi-dbd-dir,
      [AS_HELP_STRING([--with-dbi-dbd-dir=PATH],[specify location of libdbi drivers @<:@default=${libdir}/dbd@:>@])],
      [[GNC_DBD_DIR="$with_dbi_dbd_dir"
       EXTRA_SEARCH_LIBS=$GNC_DBD_DIR]],
      [[GNC_DBD_DIR=""
        # pkg-config is new in libdbi 0.9. Use it if avaiable
        # Otherwise fall back to our previous hard coded values
        pkg-config --exists dbi
        if test $? -eq 0; then
          EXTRA_SEARCH_LIBS="$(pkg-config --variable=libdir dbi)/dbd"
        else
          EXTRA_SEARCH_LIBS="/usr/lib/dbd:/usr/lib64/dbd:${libdir}/dbd"
        fi]]
       )
    case $host_os in
       win32*|mingw*)
	 LDINC="#include <windows.h>"
	 LDFUNCARGS=""
	 LDEXT=dll
	 ;;
       *)
	 LDINC="#include <dlfcn.h>"
	 LDFUNCARGS=", RTLD_NOW"
	 LDEXT=so
	 export LIBS="$lt_cv_dlopen_libs $LIBS"
	 ;;
      esac
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$EXTRA_SEARCH_LIBS"
      AC_MSG_CHECKING([Looking for at least one supported DBD module])
      AC_RUN_IFELSE([AC_LANG_PROGRAM([$LDINC],
				     [[if (!$lt_cv_dlopen("libdbdsqlite3.$LDEXT"$LDFUNCARGS)) return -1;
				     ]])],AC_MSG_RESULT([SQLite3]),
      	AC_RUN_IFELSE([AC_LANG_PROGRAM([$LDINC],
				     [[if (!$lt_cv_dlopen("libdbdmysql.$LDEXT"$LDFUNCARGS)) return -1;
					]])],AC_MSG_RESULT([MySql]),
	  AC_RUN_IFELSE([AC_LANG_PROGRAM([$LDINC],
				     [[if (!$lt_cv_dlopen("libdbdpgsql.$LDEXT"$LDFUNCARGS)) return -1;
					 ]])],AC_MSG_RESULT([Postgresql]),
        				 AC_MSG_ERROR([
Unable to find any of the supported dbd modules
(libdbdsqlite3, libdbdmysql, or libdbdpgsql) needed to actually use the SQL
backend.

If you do have them installed the problem is either that dlopen cannot
find them or that dlopen itself is not getting linked. Check config.log
to find out which.  You can add the option --with-dbi-dbd-dir pointing to
the directory in which they are located.

If you do not actually want to build with libdi add --disable-dbi
to the configure argument list and run it again.
       ]))))

    LIBDBI_LIBS=-ldbi
    _COMPONENTS="$_COMPONENTS dbi"
    LIBS=$saved_libs
  else
    AC_MSG_ERROR([

 Unable to find <dbi/dbi.h>. Either install the libdbi development
 package (such as libdbi0-dev), or switch off the database backend of
 gnucash by --disable-dbi.
 Note: If you install libdbi, you should also install its database
 drivers (such as libdbd-sqlite3 libdbd-mysql libdbd-pgsql).
])
  fi
fi
AC_SUBST(LIBDBI_LIBS)
AC_SUBST(GNC_DBD_DIR)
AM_CONDITIONAL(CUSTOM_GNC_DBD_DIR, [test x"$GNC_DBD_DIR" != "x"])
AM_CONDITIONAL(WITH_DBI, [test "x$ac_cv_header_dbi_dbi_h" = xyes])

### --------------------------------------------------------------------------
### Variables
### Set up all the initial variable values...
GNC_CONFIGDIR='${sysconfdir}/gnucash'
GNC_DOC_INSTALL_DIR='${docdir}'
GNC_INCLUDE_DIR='${includedir}/gnucash'
GNC_SCM_INSTALL_DIR='${pkgdatadir}/scm'
GNC_SHAREDIR='${pkgdatadir}'
GNC_LIBEXECDIR='${libexecdir}/gnucash'

GNC_ACCOUNTS_DIR='${GNC_SHAREDIR}/accounts'
GNC_CHECKS_DIR='${GNC_SHAREDIR}/checks'
GNC_GTKBUILDER_DIR='${GNC_SHAREDIR}/gtkbuilder'
GNC_UI_DIR='${GNC_SHAREDIR}/ui'
GNC_PIXMAP_DIR='${GNC_SHAREDIR}/pixmaps'

AC_SUBST(GNC_ACCOUNTS_DIR)
AC_SUBST(GNC_CHECKS_DIR)
AC_SUBST(GNC_CONFIGDIR)
AC_SUBST(GNC_DOC_INSTALL_DIR)
AC_SUBST(GNC_GTKBUILDER_DIR)
AC_SUBST(GNC_UI_DIR)
AC_SUBST(GNC_INCLUDE_DIR)
AC_SUBST(GNC_PIXMAP_DIR)
AC_SUBST(GNC_SCM_INSTALL_DIR)
AC_SUBST(GNC_SHAREDIR)
AC_SUBST(GNC_LIBEXECDIR)

AC_ARG_ENABLE( debug,
  [AS_HELP_STRING([--enable-debug],[compile with debugging flags set])],
  [
     # remove any optimization flags...
     CFLAGS=`echo ${CFLAGS} | sed -e 's,-O.,,g'`
     # ...except for those the user wants.
     CFLAGS="${CFLAGS} -g ${USER_OPTIMIZATION}"
     # Do the same for C++
     CXXFLAGS=`echo ${CXXFLAGS} | sed -e 's,-O.,,g'`
     CXXFLAGS="${CXXFLAGS} -g ${USER_OPTIMIZATION}"
     LDFLAGS="${LDFLAGS} -g"
     AC_DEFINE(DEBUG_MEMORY,1,[Enable debug memory])
  ],
  [     AC_DEFINE(DEBUG_MEMORY,0,[Enable debug memory])
  ])

AC_ARG_ENABLE( profile,
  [AS_HELP_STRING([--enable-profile],[compile with profiling set])],
  AM_CFLAGS="${AM_CFLAGS} -pg"
  AM_CXXFLAGS="${AM_CXXFLAGS} -pg"
  AM_LDFLAGS="${AM_LDFLAGS} -pg")

###---------------------------------------------------------------------------
### Google Profiler Support: https://code.google.com/p/gperftools/
###---------------------------------------------------------------------------
enable_google_profile=no
AC_ARG_ENABLE( google-profiler,
  [AS_HELP_STRING([--enable-google-profiler], [link in Google Performance Tools profiler; allows enabling profiling by setting $CPUPROFILE=/path/to/logfile.])],
  [AC_MSG_CHECKING([Google PerfTools Profiler])
   AC_CHECK_LIB([profiler], [enable_google_profile=yes],
      [AC_MSG_WARN([Google Profiling Enabled but the library was not found.])])
])
AM_CONDITIONAL(WITH_GOOGLE_PROFILER, [test x$enable_google_profile = xyes])

###---------------------------------------------------------------------------
### Google Test and Mock Support: https://code.google.com/p/googletest/ and
### https://code.google.com/p/googlemock/
### ---------------------------------------------------------------------------
enable_google_test=no
ac_cv_gmock_root=""
ac_cv_gmock_headers=""
ac_cv_gtest_root=""
ac_cv_gtest_headers=""
AC_ARG_ENABLE( google-test,
   [AS_HELP_STRING([--enable-google-test], [build the Google Test and Google Mock static libraries and enable C++ Unit Tests])],
   [enable_google_test=yes])
if test x$enable_google_test = xyes; then
   AC_CHECK_FILES([/usr/src/gtest/src/gtest-all.cc /usr/src/gmock/src/gmock-all.cc /usr/include/gtest/gtest.h /usr/include/gmock/gmock.h],
    [ac_cv_gtest_system_install=yes],[ac_cv_gtest_system_install=no])
fi
AC_ARG_WITH([gtest-root],
    [AS_HELP_STRING([--with-gtest-root=PATH], [location of the google test sources])],
    [ac_cv_gtest_root="$with_gtest_root"])
AC_ARG_WITH([gtest-headers],
    [AS_HELP_STRING([--with-gtest-headers=PATH], [location of the google test header files if not in gtest-root])],
    [ac_cv_gtest_headers="$with_gtest_headers"])
AC_ARG_WITH([gmock-root],
    [AS_HELP_STRING([--with-gmock-root=PATH], [location of the google mock sources])],
    [ac_cv_gmock_root="$with_gmock_root"])
AC_ARG_WITH([gmock-headers],
    [AS_HELP_STRING([--with-gmock-headers=PATH], [location of the google test header files if not in gmock-root])],
    [ac_cv_gmock_headers="$with_gmock_headers"])

if test x$enable_google_test = xyes; then
   AC_MSG_CHECKING([whether Google Test is available])
   if test -n $ac_cv_gtest_root -a -r "$ac_cv_gtest_root/src/gtest-all.cc"; then
     AC_MSG_WARN([Skipped setting ac_cv_gtest_root, value $ac_cv_gtest_root])
   else
      if test -n ${GTEST_ROOT} -a -r ${GTEST_ROOT}/src/gtest-all.cc; then
         ac_cv_gtest_root=${GTEST_ROOT}
      elif test "x$ac_cv_gtest_system_install" = "xyes"; then
         ac_cv_gtest_root="/usr/src/gtest"
      else
         ac_cv_gtest_root=""
      fi
   fi
   if test -z $ac_cv_gtest_root; then
      AC_MSG_RESULT([No gtest-root])
      enable_google_test=no
   else
      if test ! -r $ac_cv_gtest_root/include/gtest/gtest.h; then
         if test -n $ac_cv_gtest_headers -a -r $ac_cv_gtest_headers/gtest/gtest.h; then
            AC_MSG_WARN([Skipped setting ac_cv_gtest_headers, value $ac_cv_gtest_headers])
         else
            if test  -n ${GTEST_HEADERS} -a -r ${GTEST_HEADERS}/gtest/gtest.h; then
               ac_cv_gtest_headers=${GTEST_HEADERS}
            elif test "x$ac_cv_gtest_system_install" = "xyes"; then
               ac_cv_gtest_headers="/usr/include"
            else
               ac_cv_gtest_headers=""
            fi
         fi
         if test -z $ac_cv_gtest_headers; then
            AC_MSG_RESULT([No gtest-headers])
            enable_google_test=no
         fi
      else
         ac_cv_gtest_headers=$ac_cv_gtest_root/include
      fi
   fi
   if test -n $ac_cv_gmock_root -a -r "$ac_cv_gmock_root/src/gmock-all.cc"; then
      AC_MSG_WARN([Skipped setting ac_cv_gmock_root, value $ac_cv_gmock_root])
   else
      if test -n ${GMOCK_ROOT} -a -r ${GMOCK_ROOT}/src/gmock-all.cc; then
         ac_cv_gmock_root=${GMOCK_ROOT}
      elif test "x$ac_cv_gtest_system_install" = "xyes"; then
         ac_cv_gmock_root="/usr/src/gmock"
      else
         ac_cv_gmock_root=""
      fi
   fi
   if test -z $ac_cv_gmock_root; then
      AC_MSG_RESULT([No gmock-root])
      enable_google_test=no
   else
      if test ! -r $ac_cv_gmock_root/include/gmock/gmock.h; then
         if test -n $ac_cv_gmock_headers -a -r $ac_cv_gmock_headers/gmock/gmock.h; then
            AC_MSG_WARN([Skipped setting ac_cv_gmock_headers, value $ac_cv_gmock_headers])
         else
            if test -n ${GMOCK_HEADERS} -a -r ${GMOCK_HEADERS}/gmock/gmock.h; then
               ac_cv_gmock_headers=${GMOCK_HEADERS}
            elif test "x$ac_cv_gtest_system_install" = "xyes"; then
               ac_cv_gmock_headers="/usr/include"
            else
               ac_cv_gmock_headers=""
            fi
         fi
         if test -z $ac_cv_gmock_headers; then
	    AC_MSG_RESULT([No gmock-headers])
            enable_google_test=no
         fi
      else
         ac_cv_gmock_headers=$ac_cv_gmock_root/include
      fi
   fi
   if test "x$enable_google_test" = "xyes"; then
       AC_MSG_RESULT([Yes])
   fi
fi
AC_SUBST([GTEST_ROOT], [$ac_cv_gtest_root])
AC_SUBST([GTEST_HEADERS], [$ac_cv_gtest_headers])
AC_SUBST([GMOCK_ROOT], [$ac_cv_gmock_root])
AC_SUBST([GMOCK_HEADERS], [$ac_cv_gmock_headers])
AM_CONDITIONAL([WITH_GOOGLE_TEST], [test "x$enable_google_test" = "xyes"])

### --------------------------------------------------------------------------
### Register2
AC_ARG_ENABLE( register2,
  [AS_HELP_STRING([--enable-register2],[compile with register2 enabled])],
  if test x$enableval = xyes; then
    AC_DEFINE(REGISTER2_ENABLED,1,We are using Register2)
  else
    AC_DEFINE(REGISTER2_ENABLED,0,We are not using Register2)
  fi)

### --------------------------------------------------------------------------
### OFX
want_ofx=auto
have_ofx=no
AC_ARG_ENABLE( ofx,
  [AS_HELP_STRING([--enable-ofx],[compile with ofx support (needs LibOFX)])],
  [ case "$enableval" in
    yes) want_ofx=yes ;;
    no)  want_ofx=no ;;
    esac[]dnl
  ] )
AC_MSG_CHECKING(whether to use OFX)
case "${want_ofx}" in
  yes)  AC_MSG_RESULT(yes -- failure is fatal) ;;
  no)   AC_MSG_RESULT(no) ;;
  auto) AC_MSG_RESULT(yes -- automatic test) ;;
esac

if test x${want_ofx} != xno ; then
  PKG_CHECK_MODULES(LIBOFX, libofx, [have_ofx="yes"], [ have_ofx="no" ])
fi

if test x${want_ofx} = xyes && test x${have_ofx} = xno; then
  AC_MSG_ERROR([

 OFX support wanted, but libofx development libraries not found.
 Either install the libofx development package (such as libofx-dev or
 libofx-devel), or drop ofx support. You can do tis by removing
 --enable-dbi from the configure parameters.])
fi

MIGRATABLE_PREFS_OFX="/dev/null"
if test x${have_ofx} = xyes ; then
  _COMPONENTS="$_COMPONENTS ofx"
  MIGRATABLE_PREFS_OFX="$srcdir/src/import-export/ofx/migratable-prefs-ofx.xml"
fi

AM_CONDITIONAL([WITH_OFX], [test "x${have_ofx}" = xyes])
AC_SUBST_FILE([MIGRATABLE_PREFS_OFX])
### --------------------------------------------------------------------------
### MT940
AC_ARG_ENABLE( mt940,
  [AS_HELP_STRING([--enable-mt940],[obsolete, included in --enable-aqbanking])],
  if test "x$enableval" != "xno" ; then
    AC_MSG_ERROR([--enable-mt940 is obsolete -- all functionality is already included in --enable-aqbanking])
  fi)

### --------------------------------------------------------------------------
### AqBanking

AC_ARG_ENABLE( aqbanking,
  [AS_HELP_STRING([--enable-aqbanking],[compile with AqBanking support])],
  if test "x$enableval" != "xno" ; then
    want_aqbanking=yes
  fi)
AC_ARG_ENABLE( hbci,
  [AS_HELP_STRING([--enable-hbci],[obsolete, replaced by --enable-aqbanking])],
  if test "x$enableval" != "xno" ; then
    AC_MSG_ERROR([--enable-hbci is obsolete -- the option has been renamed into --enable-aqbanking])
  fi)

MIGRATABLE_PREFS_AQBANKING="/dev/null"
if test x${want_aqbanking} = xyes ;
then
  # Check for Aqbanking library
  # We require aqbanking-4.0.0 because our implementation doesn't run
  # with earlier version anymore. (libaqbanking ships with a pkgconfig
  # file since its version 1.6.1.)
  PKG_CHECK_MODULES(AQBANKING, aqbanking >= 4.0.0 gwenhywfar, , [
    AC_MSG_ERROR([Could not find aqbanking >= 4.0.0. If you use --enable-aqbanking, you MUST have aqbanking installed!])
  ])
  # For libgwenhywfar > 3.99.20, look for its library gwengui-gtk2.
  PKG_CHECK_MODULES([GWENGUI_GTK2], [gwengui-gtk2 gwenhywfar], [has_gwengui_gtk2=1], [has_gwengui_gtk2=0])

  # also check for ktoblzcheck
  AC_CHECK_HEADERS(ktoblzcheck.h)
  if test "x$ac_cv_header_ktoblzcheck_h" != xno; then
    AQBANKING_LIBS="${AQBANKING_LIBS} -lktoblzcheck"
  fi

  AS_SCRUB_INCLUDE(AQBANKING_CFLAGS)
  AC_SUBST(AQBANKING_LIBS)
  AC_SUBST(AQBANKING_CFLAGS)
  _COMPONENTS="$_COMPONENTS aqbanking"
  MIGRATABLE_PREFS_AQBANKING="$srcdir/src/import-export/aqb/migratable-prefs-aqbanking.xml"
fi
AM_CONDITIONAL([WITH_AQBANKING], [test x${want_aqbanking} = xyes])
AC_SUBST_FILE([MIGRATABLE_PREFS_AQBANKING])

AC_ARG_WITH(xdg-data-dirs,
                 [AS_HELP_STRING([--with-xdg-data-dirs],
                 [search path for system XDG data directories [default /usr/local/share/;/usr/share/]])],
                 [GNC_SYSTEM_XDG_DATA_DIRS=$withval],
                 [GNC_SYSTEM_XDG_DATA_DIRS="/usr/local/share;/usr/share"])
AC_SUBST(GNC_SYSTEM_XDG_DATA_DIRS)

AC_ARG_WITH(qt3-wizard-package,
                 [AS_HELP_STRING([--with-qt3-wizard-package=name],[name of package containing qt3-wizard (aqbanking)])],
    [QT3_WIZARD_PACKAGE=$withval], [QT3_WIZARD_PACKAGE=aqbanking])
AC_DEFINE_UNQUOTED([QT3_WIZARD_PACKAGE],["$QT3_WIZARD_PACKAGE"],[Name of package containing qt3-wizard.])

### --------------------------------------------------------------------------
### i18n

# This variable is set by GLIB_WITH_NLS, called through
# AM_GLIB_GNU_GETTEXT above. Usually it is "share", but on some
# platforms it is "lib". Make sure it is set.
if test "x$DATADIRNAME" = "x"; then
  DATADIRNAME="share"
  AC_SUBST(DATADIRNAME)
fi

dnl check for nl_langinfo(D_FMT) which is missing on FreeBSD
LANGINFO_D_FMT_CHECK

dnl Enable locale-specific tax-related information in the accounts
AC_ARG_ENABLE( locale-specific-tax,
  [AS_HELP_STRING([--enable-locale-specific-tax],[enable localized tax categories (experimental, but used by the german SKR04 account chart)])],
  AC_DEFINE(LOCALE_SPECIFIC_TAX,1,Enable the experimental locale-specific tax categories) )

dnl Make sure we have a proper gettext installed
AC_MSG_CHECKING(for a valid gettext/gmsgfmt installation)
if test "$gt_cv_have_gettext" != "yes" || test "x$GMSGFMT" = "x"; then
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([Cannot find Glib Gettext.  Maybe you need to install the gettext package?])
else
  AC_MSG_RESULT(yes - $GMSGFMT)
fi

# -------------------------------------------------------------------------
# URLs for MySQL and PostgreSQL testing
AC_ARG_WITH(test-mysql-url,
            AS_HELP_STRING([--with-test-mysql-url=URL],
                           [MySQL database URL for testing [default=none]]),
            [],[with_test_mysql_url=""])

TEST_MYSQL_URL=$with_test_mysql_url
AC_SUBST(TEST_MYSQL_URL)

AC_ARG_WITH(test-pgsql-url,
            AS_HELP_STRING([--with-test-pgsql-url=URL],
                           [PgSQL database URL for testing [default=none]]),
            [],[with_test_pgsql_url=""])

TEST_PGSQL_URL=$with_test_pgsql_url
AC_SUBST(TEST_PGSQL_URL)

### --------------------------------------------------------------------------
### help files

# Used to initialize doc-path.
AC_ARG_WITH( help-prefix,
  [AS_HELP_STRING([--with-help-prefix=PATH],[specify where to store the help files])],
  GNC_HELPDIR="$with_help_prefix",
  GNC_HELPDIR="\${datadir}")
AC_SUBST(GNC_HELPDIR)

### --------------------------------------------------------------------------
### Check for etags

AC_ARG_ENABLE( etags,
  [AS_HELP_STRING([--enable-etags],[enable automatic create of TAGS file])],
  if test x$enableval = xyes; then
     USE_ETAGS=1
  fi,
  USE_ETAGS=0)

if test x${USE_ETAGS} = x1; then
  AC_CHECK_PROG(GNC_ETAGS_FILE, etags, TAGS)
fi

AM_CONDITIONAL(GNC_ETAGS_FILE, test x${GNC_ETAGS_FILE} = xTAGS)

### --------------------------------------------------------------------------
### Check for ctags

AC_ARG_ENABLE( ctags,
  [AS_HELP_STRING([--enable-ctags],[enable automatic create of tags file])],
  if test x$enableval = xyes; then
     USE_CTAGS=1
  fi,
  USE_CTAGS=0)

if test x${USE_CTAGS} = x1; then
  AC_CHECK_PROG(GNC_CTAGS_FILE, ctags, tags)
fi

AM_CONDITIONAL(GNC_CTAGS_FILE, test x${GNC_CTAGS_FILE} = xtags)

### --------------------------------------------------------------------------
### Check for perl

# Check for perl, force version 5
AC_ARG_WITH(perl,
  [AS_HELP_STRING([--with-perl=FILE],[which perl executable to use])],
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

### ----------------------------------------------------------------------------
### Check for doxygen, mostly stolen from http://log4cpp.sourceforge.net/

AC_DEFUN([BB_ENABLE_DOXYGEN],
  [
    AC_ARG_ENABLE(doxygen, [AS_HELP_STRING([--enable-doxygen],[enable documentation generation with doxygen (auto)])])
    AC_ARG_ENABLE(dot, [AS_HELP_STRING([--enable-dot],[use 'dot' to generate graphs in doxygen (auto)])])
    AC_ARG_ENABLE(html-docs, [AS_HELP_STRING([--enable-html-docs],[enable HTML generation with doxygen (yes)])], [], [ enable_html_docs=yes])
    AC_ARG_ENABLE(latex-docs, [AS_HELP_STRING([--enable-latex-docs],[enable LaTeX documentation generation with doxygen (no)])], [], [ enable_latex_docs=no])
    if test "x$enable_doxygen" = xno; then
      enable_doc=no
    else
      AC_PATH_PROG(DOXYGEN, doxygen, , $PATH)
      if test x$DOXYGEN = x; then
        if test "x$enable_doxygen" = xyes; then
          AC_MSG_ERROR([could not find doxygen])
        fi
        enable_doc=no
      else
        enable_doc=yes
        AC_PATH_PROG(DOT, dot, , $PATH)
      fi
    fi
    AM_CONDITIONAL(DOC, test x$enable_doc = xyes)

    if test x$DOT = x; then
      if test "x$enable_dot" = xyes; then
        AC_MSG_ERROR([could not find dot])
      fi
      enable_dot=no
    else
      enable_dot=yes
    fi
    AM_CONDITIONAL(ENABLE_DOXYGEN, test x$enable_doc = xtrue)
    AC_SUBST(enable_dot)
    AC_SUBST(enable_html_docs)
    AC_SUBST(enable_latex_docs)
  ])

# check for doxygen
BB_ENABLE_DOXYGEN


### --------------------------------------------------------------------------
### Libraries
LIBS="$LIBS -lm"


### --------------------------------------------------------------------------
### Whether or not to build the GTK GUI components.
### When disabled, we don't even need to check for them!
### --------------------------------------------------------------------------

AC_ARG_ENABLE(gui,
  [AS_HELP_STRING([--disable-gui],[build without the GNOME GUI components of Gnucash])],
  [case "${enableval}" in
     yes) gnc_build_gui=true ;;
     no)  gnc_build_gui=false ;;
     *) gnc_build_gui=true ;;
   esac],
   [gnc_build_gui=true])

AM_CONDITIONAL(GNUCASH_ENABLE_GUI, test x${gnc_build_gui} = xtrue)

### --------------------------------------------------------------------------
### GNOME gui components -- built by default, but not if --enable-gui=no
### or --disable-gui is passed to autogen
### --------------------------------------------------------------------------

if test x${gnc_build_gui} = xtrue ;
then
  if test x${enable_compile_warnings} = x ;
  then
    enable_compile_warnings="yes"
  fi
#Save CFLAGS so that we can put the warnings in AM_CFLAGS
  __cflags="$CFLAGS"
  CFLAGS=
  GNOME_COMPILE_WARNINGS
  AM_CFLAGS="$CFLAGS"
  CFLAGS="$__cflags"

  PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.24.0)
  AS_SCRUB_INCLUDE(GTK_CFLAGS)
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)

  PKG_CHECK_MODULES(GNOMECANVAS, libgnomecanvas-2.0)
  AS_SCRUB_INCLUDE(GNOMECANVAS_CFLAGS)
  AC_SUBST(GNOMECANVAS_CFLAGS)
  AC_SUBST(GNOMECANVAS_LIBS)

  PKG_CHECK_MODULES(GDK_PIXBUF, gdk-pixbuf-2.0)
  AS_SCRUB_INCLUDE(GDK_PIXBUF_CFLAGS)
  AC_SUBST(GDK_PIXBUF_CFLAGS)
  AC_SUBST(GDK_PIXBUF_LIBS)

   # checks for goffice
  PKG_CHECK_MODULES(GOFFICE, libgoffice-0.8 >= 0.7.0, [goffice=1], [AC_MSG_ERROR([Cannot find libgoffice.>= 0.7.0])])
  AS_SCRUB_INCLUDE(GOFFICE_CFLAGS)
  AC_SUBST(GOFFICE_CFLAGS)
  AC_SUBST(GOFFICE_LIBS)

  ### --------------------------------------------------------------------------
  ### determine the HTML engine

  AC_MSG_CHECKING([the html engine])
  AC_ARG_WITH(html_engine,
              AS_HELP_STRING([--with-html-engine=@<:@webkit@:>@],
                             [Obsolete: Only webkit is used as HTML engine now]),
              [],[with_html_engine="webkit"])

  AC_MSG_RESULT([$with_html_engine])

  case "$with_html_engine" in

    gtkhtml)
      # No more gtkhtml in gnucash-2.5.0
      AC_MSG_ERROR([gtkhtml is no longer sufficient for gnucash, as we now require the CSS/javascript features using webkit. Please remove your --with-html-engine argument from configure.])
      ;;

    webkit)
      PKG_CHECK_MODULES(WEBKIT, webkit-1.0 >= "1.2")
      case "$platform" in
        win32)
          # 1.1.90 has both functions
          AC_DEFINE(HAVE_WEBKIT_WEB_VIEW_LOAD_URI,1,[webkit_web_view_load_uri exists])
          AC_DEFINE(HAVE_WEBKIT_WEB_FRAME_PRINT_FULL,1,[webkit_web_frame_print_full exists])
          ;;
        *)
          # Unsure - check functions exist
          oLIBS="$LIBS"
          LIBS="$LIBS ${WEBKIT_LIBS}"
          AC_CHECK_FUNCS(webkit_web_view_load_uri)
          LIBS="$oLIBS"
          AC_MSG_CHECKING(for webkit_web_frame_print_full)
          saved_CFLAGS="${CFLAGS}"
          saved_LIBS="${LIBS}"
          CFLAGS="${CFLAGS} ${WEBKIT_CFLAGS}"
          LIBS="${LIBS} ${WEBKIT_LIBS}"
          AC_LINK_IFELSE(
              [AC_LANG_PROGRAM(
                  [[#include <webkit/webkit.h>]],
                  [[webkit_web_frame_print_full( 0, 0, 0, 0 );]])],
                  [AC_MSG_RESULT(yes)
                   AC_DEFINE(HAVE_WEBKIT_WEB_FRAME_PRINT_FULL,1,[webkit_web_frame_print_full exists])],
                  [AC_MSG_RESULT(no)])
          CFLAGS="${saved_CFLAGS}"
          LIBS="${saved_LIBS}"
          ;;
        esac
        ;;

    *) AC_MSG_ERROR([Invalid HTML engine: must be webkit]) ;;
  esac

  AC_SUBST(WEBKIT_CFLAGS)
  AC_SUBST(WEBKIT_LIBS)

  dnl if Mac OSX, also scrub /sw/include
  dnl GIVEN_CFLAGS=$(echo $GIVEN_CFLAGS | sed -e "s;-I/sw/include ;;" | sed -e "s;-I/sw/include$;;")

  ###-----------------------------------------------------------------------
  ## Find a suitable password store

  case $host_os in
    darwin*)
      AC_DEFINE(HAVE_OSX_KEYCHAIN,1,[System has an OS X Key chain])
      ;;
    *)
      PKG_CHECK_MODULES(GNOME_KEYRING, gnome-keyring-1 >= "0.6",
          [AC_DEFINE(HAVE_GNOME_KEYRING,1,[System has gnome-keyring 0.6 or better])],
          [AC_DEFINE(HAVE_NO_KEYRING,1,[System has no suitable keyring service])])

      PKG_CHECK_MODULES(LIBSECRET, libsecret-1 >= "0.18",
          [AC_DEFINE(HAVE_LIBSECRET,1,[System has libsecret 0.18 or better])],
          [AC_DEFINE(HAVE_NO_LIBSECRET,1,[System has no suitable libsecret service])])
      AC_SUBST(LIBSECRET_CFLAGS)
      AC_SUBST(LIBSECRET_LIBS)
      ;;
  esac

  ### ----------------------------------------------------------------------

  AC_ARG_ENABLE( efence,
    [AS_HELP_STRING([--enable-efence],[link using efence])],
    if test x$enableval = xyes; then
      EFENCE_LIBS="-lefence"
      USE_EFENCE=1
      AC_DEFINE(USE_EFENCE,,We are using EFence)
    fi,
    USE_EFENCE=0
    EFENCE_LIBS="")
  AC_SUBST(EFENCE_LIBS)

  ### ----------------------------------------------------------------------
  ## For now, we just presume you're using the GNOME version.  The other
  ## UI's haven't been carried over during the automake transition.  At
  ## some point, if it's deemed worthwhile, they can be resurrected...

  GNOME=1
  AC_DEFINE(GNOME,,using GNOME)
  AM_CXXFLAGS="${AM_CXXFLAGS} -Wall"

else
  # GNOME_COMPILE_WARNINGS will add -Wall; no need to set it again.
  # also, only add it for GCC.
  if test ${GCC}x = yesx
  then
    # We should always see these errors...
    AM_CFLAGS="${AM_CFLAGS} -Wall"
    AM_CXXFLAGS="${AM_CXXFLAGS} -Wall"
  fi

fi
### End of gui-only checks

###-------------------------------------------------------------------------
### Selectively disable deprecated bits of glib/gdk/gtk/gnome
###-------------------------------------------------------------------------

allow_deprecated=true
AC_ARG_ENABLE(deprecated-glib,
  [AS_HELP_STRING([--disable-deprecated-glib],[don't use deprecated glib functions])],
  [case "${enableval}" in
     no) allow_deprecated=false ;;
     *)  allow_deprecated=true ;;
   esac]
  )
if test x${allow_deprecated} != "xtrue"
then
  # FIXME: This define is unused; this block can be removed
  AC_DEFINE(G_DISABLE_DEPRECATED,1, [Don't use deprecated glib functions])
fi


# default: Allow deprecated gtk functions
allow_deprecated=true
AC_ARG_ENABLE(deprecated-gtk,
  [AS_HELP_STRING([--disable-deprecated-gtk],[don't use deprecated gtk, gdk or gdk-pixbuf functions])],
  [case "${enableval}" in
     no) allow_deprecated=false ;;
     *)  allow_deprecated=true ;;
   esac]
  )
if test x${allow_deprecated} != "xtrue"
then
  # FIXME: This define is unused; this block can be removed
  AC_DEFINE(GTK_DISABLE_DEPRECATED,1, [Don't use deprecated gtk functions])
  AC_DEFINE(GDK_DISABLE_DEPRECATED,1, [Don't use deprecated gdk functions])
  AC_DEFINE(GDK_PIXBUF_DISABLE_DEPRECATED,1, [Don't use deprecated gdk-pixbuf functions])
fi


allow_deprecated=true
AC_ARG_ENABLE(deprecated-gnome,
  [AS_HELP_STRING([--disable-deprecated-gnome],[don't use deprecated gnome functions])],
  [case "${enableval}" in
     no) allow_deprecated=false ;;
     *)  allow_deprecated=true ;;
   esac]
  )
if test x${allow_deprecated} != "xtrue"
then
  # FIXME: This define is unused; this block can be removed
  AC_DEFINE(GNOME_DISABLE_DEPRECATED,1, [Don't use deprecated gnome functions])
fi

###-------------------------------------------------------------------------
### Stuff from Mac OS X Port
###-------------------------------------------------------------------------

AC_CHECK_FUNCS(pthread_mutex_init)
case $host_os in
  darwin*)
    AC_REPLACE_FUNCS(localtime_r gmtime_r)
    AC_LIBOBJ(strptime)
    ;;
  *)
    AC_REPLACE_FUNCS(strptime localtime_r gmtime_r strfmon)
    ;;
esac

if test x$am_cv_val_LC_MESSAGES = "xno"; then
  LC_MESSAGES_ENUM="LC_ALL"
else
  LC_MESSAGES_ENUM="LC_MESSAGES"
fi
AC_SUBST(LC_MESSAGES_ENUM)

###--------------------------------------------------------
### Make Python plugin and bindings optional
###--------------------------------------------------------

AC_ARG_ENABLE(python,
  [AS_HELP_STRING([--enable-python],[enable python plugin and bindings])],
  [case "${enableval}" in
     yes) enable_python=true ;;
     no) enable_python=false ;;
     *) enable_python=false    # default: false
   esac],
  [enable_python=false] # default: false
  )
if test x${enable_python} = "xtrue"
then
  # Check for Python. We require python-2.4

  AM_PATH_PYTHON(2.4,,[
	   AC_MSG_ERROR([Could not find python >= 2.4. If you use --enable-python, you MUST have python installed!])
    ])
  AC_PYTHON_DEVEL(>= '2.4')
  if test "x${PYTHON}" = "x"
  then
	   AC_MSG_ERROR([Could not find Python development files. Make sure that the correct python-devel package is installed.])
  fi
  SWIG_PYTHON
  _COMPONENTS="$_COMPONENTS python"
fi
AM_CONDITIONAL(WITH_PYTHON, [test x${enable_python} = xtrue])

AC_ARG_ENABLE(python-bindings,
  [AS_HELP_STRING([--enable-python-bindings],[obsolete: replaced by --enable-python])],
  if test "x$enableval" != "xno" ; then
    AC_MSG_ERROR([--enable-python-bindings is obsolete: The option has been renamed into --enable-python])
  fi)


###-------------------------------------------------------------------------
### Additional compiler warnings (or not) if we're running GCC
###-------------------------------------------------------------------------

# This has to come after AC_PROG_CC _AND_ GNOME_COMPILE_WARNINGS
AC_LANG([C++])
AX_CHECK_COMPILE_FLAG([-Wno-deprecated-register],
    [AM_CXXFLAGS="${AM_CXXFLAGS} -Wno-unused -Wno-deprecated-register"],
    [AM_CXXFLAGS="${AM_CXXFLAGS} -Wno-unused"], [-Werror])
AC_LANG([C])
AC_MSG_CHECKING(what extra warning flags to pass to the C compiler)
if test ${GCC}x = yesx
then
  warnFLAGS=
  warnXXFLAGS=
  #We have a lot of unused variables. Don't warn until someone gets
  #around to cleaning them up.
  AM_CFLAGS="${AM_CFLAGS} -Wno-unused"
  # other flags...
  # These next two are included in the GNOME_COMPILE_WARNINGS
  #warnFLAGS="${warnFLAGS} -Wmissing-prototypes"
  #warnFLAGS="${warnFLAGS} -Wmissing-declarations"
  #warnFLAGS="${warnFLAGS} -Werror-implicit-function-declaration" # In -Wall

  # error-on-warning should not be active in (stable) release tarballs
  if test "x$BUILDING_FROM_SCM" = "xyes"
  then
    # This code is from svn/svk/git/bzr, so enable error-on-warning
    error_on_warning_as_default="yes"
  else
    # This is from a tarball, so disable error-on-warning
    error_on_warning_as_default="no"
  fi

  # Enable error-on-warning by default -- I'm tired of fixing other
  # people's missing #includes, etc.
  AC_ARG_ENABLE(error-on-warning,
    [AS_HELP_STRING([--disable-error-on-warning],[disable treating compile warnings as errors])],
    [case "${enableval}" in
       yes) warnFLAGS="${warnFLAGS} -Werror" ; warnXXFLAGS="${warnXXFLAGS} -Werror" ; gnc_error_on_warning=yes ;;
       no)  gnc_error_on_warning=no ;;
       *) AC_MSG_ERROR(bad value ${enableval} for --enable-error-on-warning) ;;
     esac],
    [ if test "${error_on_warning_as_default}" = "yes"; then
        warnFLAGS="${warnFLAGS} -Werror";
        warnXXFLAGS="${warnXXFLAGS} -Werror";
        gnc_error_on_warning=auto
      else
        gnc_error_on_warning=no
      fi
    ])

  warnFLAGS="${warnFLAGS} -Wdeclaration-after-statement -Wno-pointer-sign"
  # rpmbuild on FC4 forces this flag. Can't hurt to always compile with it.
  cflags_save="$CFLAGS"
  CFLAGS="$AM_CFLAGS $CFLAGS -Werror -D_FORTIFY_SOURCE=2"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
	 ]],
	 [[ printf( "%s\n", "Hello World!");]])],
	 [warnFLAGS="${warnFLAGS} -D_FORTIFY_SOURCE=2"])
  CFLAGS="$cflags_save"

  AM_CFLAGS="${warnFLAGS} ${AM_CFLAGS}"
  AM_CXXFLAGS="${warnXXFLAGS} ${AM_CXXFLAGS}"
else
  warnFLAGS=none
fi
AC_SUBST(AM_CFLAGS)
AC_SUBST(AM_CXXFLAGS)
AC_MSG_RESULT($warnFLAGS)

### --------------------------------------------------------------------------
### Adjustments -- especially executables that aren't generated via
### makefiles, so that we don't have an opportunity to adjust them
### there.

chmod u+x ${srcdir}/src/bin/generate-gnc-script

### --------------------------------------------------------------------------
### Makefile creation

# This is necessary so that .o files in LIBOBJS are also built via
# the ANSI2KNR-filtering rules.
LIBOBJS_SEDSCRIPT="s,\.[[^.]]* ,$U&,g;s,\.[[^.]]*\$\$,$U&,"
AC_SUBST(LIBOBJS_SEDSCRIPT)

# Don't add empty lines in the following macro, they
# will cause the Windows build to fail if using a git repo as source
AC_CONFIG_FILES(
  po/Makefile.in
  dnl # Makefiles
  Makefile
  accounts/Makefile
  accounts/C/Makefile
  accounts/cs/Makefile
  accounts/da/Makefile
  accounts/de_AT/Makefile
  accounts/de_CH/Makefile
  accounts/de_DE/Makefile
  accounts/el_GR/Makefile
  accounts/en_GB/Makefile
  accounts/es_ES/Makefile
  accounts/es_MX/Makefile
  accounts/fi_FI/Makefile
  accounts/fr_CA/Makefile
  accounts/fr_CH/Makefile
  accounts/fr_FR/Makefile
  accounts/hu_HU/Makefile
  accounts/it/Makefile
  accounts/ja/Makefile
  accounts/ko/Makefile
  accounts/lv/Makefile
  accounts/nb/Makefile
  accounts/nl/Makefile
  accounts/pl/Makefile
  accounts/pt_BR/Makefile
  accounts/pt_PT/Makefile
  accounts/ru/Makefile
  accounts/sk/Makefile
  accounts/sv_AX/Makefile
  accounts/sv_FI/Makefile
  accounts/sv_SE/Makefile
  accounts/tr_TR/Makefile
  accounts/zh_CN/Makefile
  accounts/zh_HK/Makefile
  accounts/zh_TW/Makefile
  checks/Makefile
  doc/Makefile
  doc/examples/Makefile
  intl-scm/Makefile
  lib/Makefile
  lib/libc/Makefile
  lib/stf/Makefile
  packaging/Makefile
  src/Makefile
  src/app-utils/Makefile
  src/app-utils/test/Makefile
  src/backend/Makefile
  src/backend/dbi/Makefile
  src/backend/dbi/test/Makefile
  src/backend/xml/Makefile
  src/backend/xml/test/Makefile
  src/backend/xml/test/test-files/Makefile
  src/backend/xml/test/test-files/xml2/Makefile
  src/backend/sql/Makefile
  src/backend/sql/test/Makefile
  src/bin/Makefile
  src/bin/overrides/Makefile
  src/bin/test/Makefile
  src/core-utils/Makefile
  src/core-utils/test/Makefile
  src/debug/Makefile
  src/debug/valgrind/Makefile
  src/doc/Makefile
  src/doc/design/Makefile
  src/doc/xml/Makefile
  src/engine/Makefile
  src/engine/test/Makefile
  src/engine/test-core/Makefile
  src/gnc-module/Makefile
  src/gnc-module/test/Makefile
  src/gnc-module/test/mod-foo/Makefile
  src/gnc-module/test/mod-bar/Makefile
  src/gnc-module/test/mod-baz/Makefile
  src/gnc-module/test/misc-mods/Makefile
  src/gnome/Makefile
  src/gnome/gtkbuilder/Makefile
  src/gnome/gschemas/Makefile
  src/gnome/ui/Makefile
  src/gnome-utils/Makefile
  src/gnome-utils/gtkbuilder/Makefile
  src/gnome-utils/gschemas/Makefile
  src/gnome-utils/test/Makefile
  src/gnome-utils/ui/Makefile
  src/gnome-search/Makefile
  src/html/Makefile
  src/import-export/Makefile
  src/import-export/test/Makefile
  src/import-export/ofx/gschemas/Makefile
  src/import-export/qif-imp/Makefile
  src/import-export/qif/Makefile
  src/import-export/qif/test/Makefile
  src/import-export/qif-imp/gschemas/Makefile
  src/import-export/qif-imp/test/Makefile
  src/import-export/gschemas/Makefile
  src/import-export/ofx/Makefile
  src/import-export/ofx/test/Makefile
  src/import-export/csv-imp/Makefile
  src/import-export/csv-imp/gschemas/Makefile
  src/import-export/csv-exp/Makefile
  src/import-export/csv-exp/gschemas/Makefile
  src/import-export/log-replay/Makefile
  src/import-export/aqb/Makefile
  src/import-export/aqb/gschemas/Makefile
  src/import-export/aqb/test/Makefile
  src/libqof/Makefile
  src/libqof/qof/Makefile
  src/libqof/qof/test/Makefile
  src/optional/Makefile
  src/optional/gtkmm/Makefile
  src/optional/gtkmm/test/Makefile
  src/optional/python-bindings/Makefile
  src/optional/python-bindings/tests/Makefile
  src/pixmaps/Makefile
  src/python/Makefile
  src/python/pycons/Makefile
  src/quotes/Makefile
  src/register/Makefile
  src/register/ledger-core/Makefile
  src/register/ledger-core/test/Makefile
  src/register/register-core/Makefile
  src/register/register-core/test/Makefile
  src/register/register-gnome/Makefile
  src/register/register-gnome/test/Makefile
  src/report/Makefile
  src/report/report-gnome/Makefile
  src/report/report-gnome/test/Makefile
  src/report/report-system/Makefile
  src/report/report-system/test/Makefile
  src/report/standard-reports/Makefile
  src/report/standard-reports/test/Makefile
  src/report/business-reports/Makefile
  src/report/locale-specific/Makefile
  src/report/locale-specific/us/Makefile
  src/report/locale-specific/us/test/Makefile
  src/report/stylesheets/Makefile
  src/report/stylesheets/test/Makefile
  src/report/utility-reports/Makefile
  src/report/utility-reports/test/Makefile
  src/report/jqplot/Makefile
  src/scm/Makefile
  src/scm/gnumeric/Makefile
  src/tax/Makefile
  src/tax/us/Makefile
  src/tax/us/test/Makefile
  src/test-core/Makefile
  src/business/Makefile
  src/business/business-gnome/Makefile
  src/business/business-gnome/gtkbuilder/Makefile
  src/business/business-gnome/gschemas/Makefile
  src/business/business-gnome/ui/Makefile
  src/business/business-ledger/Makefile
  dnl # Stuff for bill/invoice import plugin
  src/plugins/Makefile
  src/plugins/bi_import/Makefile
  src/plugins/bi_import/gtkbuilder/Makefile
  src/plugins/bi_import/ui/Makefile
  dnl # Stuff for customer import.
  src/plugins/customer_import/Makefile
  src/plugins/customer_import/gtkbuilder/Makefile
  src/plugins/customer_import/ui/Makefile
  dnl # non-makefiles
  src/bin/gnucash.rc
  src/app-utils/migratable-prefs.xml
  src/engine/iso-currencies-to-c
  src/gnome/gnucash.desktop.in
  dnl # GSettings schema files
  src/business/business-gnome/gschemas/org.gnucash.dialogs.business.gschema.xml.in
  src/gnome/gschemas/org.gnucash.dialogs.checkprinting.gschema.xml.in
  src/gnome/gschemas/org.gnucash.dialogs.commodities.gschema.xml.in
  src/gnome/gschemas/org.gnucash.dialogs.gschema.xml.in
  src/gnome/gschemas/org.gnucash.dialogs.reconcile.gschema.xml.in
  src/gnome/gschemas/org.gnucash.dialogs.sxs.gschema.xml.in
  src/gnome/gschemas/org.gnucash.dialogs.totd.gschema.xml.in
  src/gnome/gschemas/org.gnucash.gschema.xml.in
  src/gnome/gschemas/org.gnucash.window.pages.account.tree.gschema.xml.in
  src/gnome/gschemas/org.gnucash.window.pages.gschema.xml.in
  src/gnome-utils/gschemas/org.gnucash.history.gschema.xml.in
  src/gnome-utils/gschemas/org.gnucash.warnings.gschema.xml.in
  src/import-export/aqb/gschemas/org.gnucash.dialogs.import.hbci.gschema.xml.in
  src/import-export/csv-exp/gschemas/org.gnucash.dialogs.export.csv.gschema.xml.in
  src/import-export/csv-imp/gschemas/org.gnucash.dialogs.import.csv.gschema.xml.in
  src/import-export/gschemas/org.gnucash.dialogs.import.generic.gschema.xml.in
  src/import-export/ofx/gschemas/org.gnucash.dialogs.import.ofx.gschema.xml.in
  src/import-export/qif-imp/gschemas/org.gnucash.dialogs.import.qif.gschema.xml.in
  dnl # Please read doc/build-system before adding *anything* here
  ,
  dnl # init-commands go here
)
# A few files need extra actions at creation time
AC_CONFIG_FILES([src/gnc-test-env], [chmod u+x src/gnc-test-env])
AC_CONFIG_FILES([src/bin/overrides/gnucash-make-guids], [chmod u+x src/bin/overrides/gnucash-make-guids])
LDFLAGS="${LDFLAGS} ${NOUNDEF}"
AC_OUTPUT


AC_MSG_RESULT([
  Options detected/selected
  -------------------------
  gnucash version ...... : $VERSION
  Build for host ....... : $host
  Optional components... : $_COMPONENTS
  Extra Warnings ....... : $warnFLAGS
  CPPFLAGS ............. : $CPPFLAGS
  CFLAGS ............... : $CFLAGS
  CXXFLAGS ............. : $CXXFLAGS
  LDFLAGS .............. : $LDFLAGS
  prefix................ : ${prefix}


])
