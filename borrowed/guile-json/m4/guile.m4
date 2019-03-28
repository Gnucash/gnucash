## Autoconf macros for working with Guile.
##
##   Copyright (C) 1998,2001, 2006, 2010, 2012, 2013, 2014 Free Software Foundation, Inc.
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public License
## as published by the Free Software Foundation; either version 3 of
## the License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA

# serial 11

## Index
## -----
##
## GUILE_PKG -- find Guile development files
## GUILE_PROGS -- set paths to Guile interpreter, config and tool programs
## GUILE_FLAGS -- set flags for compiling and linking with Guile
## GUILE_SITE_DIR -- find path to Guile "site" directories
## GUILE_CHECK -- evaluate Guile Scheme code and capture the return value
## GUILE_MODULE_CHECK -- check feature of a Guile Scheme module
## GUILE_MODULE_AVAILABLE -- check availability of a Guile Scheme module
## GUILE_MODULE_REQUIRED -- fail if a Guile Scheme module is unavailable
## GUILE_MODULE_EXPORTS -- check if a module exports a variable
## GUILE_MODULE_REQUIRED_EXPORT -- fail if a module doesn't export a variable

## Code
## ----

## NOTE: Comments preceding an AC_DEFUN (starting from "Usage:") are massaged
## into doc/ref/autoconf-macros.texi (see Makefile.am in that directory).

# GUILE_PKG -- find Guile development files
#
# Usage: GUILE_PKG([VERSIONS])
#
# This macro runs the @code{pkg-config} tool to find development files
# for an available version of Guile.
#
# By default, this macro will search for the latest stable version of
# Guile (e.g. 2.2), falling back to the previous stable version
# (e.g. 2.0) if it is available.  If no guile-@var{VERSION}.pc file is
# found, an error is signalled.  The found version is stored in
# @var{GUILE_EFFECTIVE_VERSION}.
#
# If @code{GUILE_PROGS} was already invoked, this macro ensures that the
# development files have the same effective version as the Guile
# program.
#
# @var{GUILE_EFFECTIVE_VERSION} is marked for substitution, as by
# @code{AC_SUBST}.
#
AC_DEFUN([GUILE_PKG],
 [PKG_PROG_PKG_CONFIG
  _guile_versions_to_search="m4_default([$1], [2.2 2.0 1.8])"
  if test -n "$GUILE_EFFECTIVE_VERSION"; then
    _guile_tmp=""
    for v in $_guile_versions_to_search; do
      if test "$v" = "$GUILE_EFFECTIVE_VERSION"; then
        _guile_tmp=$v
      fi
    done
    if test -z "$_guile_tmp"; then
      AC_MSG_FAILURE([searching for guile development files for versions $_guile_versions_to_search, but previously found $GUILE version $GUILE_EFFECTIVE_VERSION])
    fi
    _guile_versions_to_search=$GUILE_EFFECTIVE_VERSION
  fi
  GUILE_EFFECTIVE_VERSION=""
  _guile_errors=""
  for v in $_guile_versions_to_search; do
    if test -z "$GUILE_EFFECTIVE_VERSION"; then
      AC_MSG_NOTICE([checking for guile $v])
      PKG_CHECK_EXISTS([guile-$v], [GUILE_EFFECTIVE_VERSION=$v], [])
    fi
  done

  if test -z "$GUILE_EFFECTIVE_VERSION"; then
    AC_MSG_ERROR([
No Guile development packages were found.

Please verify that you have Guile installed.  If you installed Guile
from a binary distribution, please verify that you have also installed
the development packages.  If you installed it yourself, you might need
to adjust your PKG_CONFIG_PATH; see the pkg-config man page for more.
])
  fi
  AC_MSG_NOTICE([found guile $GUILE_EFFECTIVE_VERSION])
  AC_SUBST([GUILE_EFFECTIVE_VERSION])
 ])

# GUILE_FLAGS -- set flags for compiling and linking with Guile
#
# Usage: GUILE_FLAGS
#
# This macro runs the @code{pkg-config} tool to find out how to compile
# and link programs against Guile.  It sets four variables:
# @var{GUILE_CFLAGS}, @var{GUILE_LDFLAGS}, @var{GUILE_LIBS}, and
# @var{GUILE_LTLIBS}.
#
# @var{GUILE_CFLAGS}: flags to pass to a C or C++ compiler to build code that
# uses Guile header files.  This is almost always just one or more @code{-I}
# flags.
#
# @var{GUILE_LDFLAGS}: flags to pass to the compiler to link a program
# against Guile.  This includes @code{-lguile-@var{VERSION}} for the
# Guile library itself, and may also include one or more @code{-L} flag
# to tell the compiler where to find the libraries.  But it does not
# include flags that influence the program's runtime search path for
# libraries, and will therefore lead to a program that fails to start,
# unless all necessary libraries are installed in a standard location
# such as @file{/usr/lib}.
#
# @var{GUILE_LIBS} and @var{GUILE_LTLIBS}: flags to pass to the compiler or to
# libtool, respectively, to link a program against Guile.  It includes flags
# that augment the program's runtime search path for libraries, so that shared
# libraries will be found at the location where they were during linking, even
# in non-standard locations.  @var{GUILE_LIBS} is to be used when linking the
# program directly with the compiler, whereas @var{GUILE_LTLIBS} is to be used
# when linking the program is done through libtool.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_FLAGS],
 [AC_REQUIRE([GUILE_PKG])
  PKG_CHECK_MODULES(GUILE, [guile-$GUILE_EFFECTIVE_VERSION])

  dnl GUILE_CFLAGS and GUILE_LIBS are already defined and AC_SUBST'd by
  dnl PKG_CHECK_MODULES.  But GUILE_LIBS to pkg-config is GUILE_LDFLAGS
  dnl to us.

  GUILE_LDFLAGS=$GUILE_LIBS

  dnl Determine the platform dependent parameters needed to use rpath.
  dnl AC_LIB_LINKFLAGS_FROM_LIBS is defined in gnulib/m4/lib-link.m4 and needs
  dnl the file gnulib/build-aux/config.rpath.
  AC_LIB_LINKFLAGS_FROM_LIBS([GUILE_LIBS], [$GUILE_LDFLAGS], [])
  GUILE_LIBS="$GUILE_LDFLAGS $GUILE_LIBS"
  AC_LIB_LINKFLAGS_FROM_LIBS([GUILE_LTLIBS], [$GUILE_LDFLAGS], [yes])
  GUILE_LTLIBS="$GUILE_LDFLAGS $GUILE_LTLIBS"

  AC_SUBST([GUILE_EFFECTIVE_VERSION])
  AC_SUBST([GUILE_CFLAGS])
  AC_SUBST([GUILE_LDFLAGS])
  AC_SUBST([GUILE_LIBS])
  AC_SUBST([GUILE_LTLIBS])
 ])

# GUILE_SITE_DIR -- find path to Guile site directories
#
# Usage: GUILE_SITE_DIR
#
# This looks for Guile's "site" directories.  The variable @var{GUILE_SITE} will
# be set to Guile's "site" directory for Scheme source files (usually something
# like PREFIX/share/guile/site).  @var{GUILE_SITE_CCACHE} will be set to the
# directory for compiled Scheme files also known as @code{.go} files
# (usually something like
# PREFIX/lib/guile/@var{GUILE_EFFECTIVE_VERSION}/site-ccache).
# @var{GUILE_EXTENSION} will be set to the directory for compiled C extensions
# (usually something like
# PREFIX/lib/guile/@var{GUILE_EFFECTIVE_VERSION}/extensions). The latter two
# are set to blank if the particular version of Guile does not support
# them.  Note that this macro will run the macros @code{GUILE_PKG} and
# @code{GUILE_PROGS} if they have not already been run.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_SITE_DIR],
 [AC_REQUIRE([GUILE_PKG])
  AC_REQUIRE([GUILE_PROGS])
  AC_MSG_CHECKING(for Guile site directory)
  GUILE_SITE=`$PKG_CONFIG --print-errors --variable=sitedir guile-$GUILE_EFFECTIVE_VERSION`
  AC_MSG_RESULT($GUILE_SITE)
  if test "$GUILE_SITE" = ""; then
     AC_MSG_FAILURE(sitedir not found)
  fi
  AC_SUBST(GUILE_SITE)
  AC_MSG_CHECKING([for Guile site-ccache directory using pkgconfig])
  GUILE_SITE_CCACHE=`$PKG_CONFIG --variable=siteccachedir guile-$GUILE_EFFECTIVE_VERSION`
  if test "$GUILE_SITE_CCACHE" = ""; then
    AC_MSG_RESULT(no)
    AC_MSG_CHECKING([for Guile site-ccache directory using interpreter])
    GUILE_SITE_CCACHE=`$GUILE -c "(display (if (defined? '%site-ccache-dir) (%site-ccache-dir) \"\"))"`
    if test $? != "0" -o "$GUILE_SITE_CCACHE" = ""; then
      AC_MSG_RESULT(no)
      GUILE_SITE_CCACHE=""
      AC_MSG_WARN([siteccachedir not found])
    fi
  fi
  AC_MSG_RESULT($GUILE_SITE_CCACHE)
  AC_SUBST([GUILE_SITE_CCACHE])
  AC_MSG_CHECKING(for Guile extensions directory)
  GUILE_EXTENSION=`$PKG_CONFIG --print-errors --variable=extensiondir guile-$GUILE_EFFECTIVE_VERSION`
  AC_MSG_RESULT($GUILE_EXTENSION)
  if test "$GUILE_EXTENSION" = ""; then
    GUILE_EXTENSION=""
    AC_MSG_WARN(extensiondir not found)
  fi
  AC_SUBST(GUILE_EXTENSION)
 ])

# GUILE_PROGS -- set paths to Guile interpreter, config and tool programs
#
# Usage: GUILE_PROGS([VERSION])
#
# This macro looks for programs @code{guile} and @code{guild}, setting
# variables @var{GUILE} and @var{GUILD} to their paths, respectively.
# The macro will attempt to find @code{guile} with the suffix of
# @code{-X.Y}, followed by looking for it with the suffix @code{X.Y}, and
# then fall back to looking for @code{guile} with no suffix. If
# @code{guile} is still not found, signal an error. The suffix, if any,
# that was required to find @code{guile} will be used for @code{guild}
# as well.
#
# By default, this macro will search for the latest stable version of
# Guile (e.g. 2.2). x.y or x.y.z versions can be specified. If an older
# version is found, the macro will signal an error.
#
# The effective version of the found @code{guile} is set to
# @var{GUILE_EFFECTIVE_VERSION}.  This macro ensures that the effective
# version is compatible with the result of a previous invocation of
# @code{GUILE_FLAGS}, if any.
#
# As a legacy interface, it also looks for @code{guile-config} and
# @code{guile-tools}, setting @var{GUILE_CONFIG} and @var{GUILE_TOOLS}.
#
# The variables are marked for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_PROGS],
 [_guile_required_version="m4_default([$1], [$GUILE_EFFECTIVE_VERSION])"
  if test -z "$_guile_required_version"; then
    _guile_required_version=2.2
  fi

  _guile_candidates=guile
  _tmp=
  for v in `echo "$_guile_required_version" | tr . ' '`; do
    if test -n "$_tmp"; then _tmp=$_tmp.; fi
    _tmp=$_tmp$v
    _guile_candidates="guile-$_tmp guile$_tmp $_guile_candidates"
  done

  AC_PATH_PROGS(GUILE,[$_guile_candidates])
  if test -z "$GUILE"; then
      AC_MSG_ERROR([guile required but not found])
  fi

  _guile_suffix=`echo "$GUILE" | sed -e 's,^.*/guile\(.*\)$,\1,'`
  _guile_effective_version=`$GUILE -c "(display (effective-version))"`
  if test -z "$GUILE_EFFECTIVE_VERSION"; then
    GUILE_EFFECTIVE_VERSION=$_guile_effective_version
  elif test "$GUILE_EFFECTIVE_VERSION" != "$_guile_effective_version"; then
    AC_MSG_ERROR([found development files for Guile $GUILE_EFFECTIVE_VERSION, but $GUILE has effective version $_guile_effective_version])
  fi

  _guile_major_version=`$GUILE -c "(display (major-version))"`
  _guile_minor_version=`$GUILE -c "(display (minor-version))"`
  _guile_micro_version=`$GUILE -c "(display (micro-version))"`
  _guile_prog_version="$_guile_major_version.$_guile_minor_version.$_guile_micro_version"

  AC_MSG_CHECKING([for Guile version >= $_guile_required_version])
  _major_version=`echo $_guile_required_version | cut -d . -f 1`
  _minor_version=`echo $_guile_required_version | cut -d . -f 2`
  _micro_version=`echo $_guile_required_version | cut -d . -f 3`
  if test "$_guile_major_version" -gt "$_major_version"; then
    true
  elif test "$_guile_major_version" -eq "$_major_version"; then
    if test "$_guile_minor_version" -gt "$_minor_version"; then
      true
    elif test "$_guile_minor_version" -eq "$_minor_version"; then
      if test -n "$_micro_version"; then
        if test "$_guile_micro_version" -lt "$_micro_version"; then
          AC_MSG_ERROR([Guile $_guile_required_version required, but $_guile_prog_version found])
        fi
      fi
    elif test "$GUILE_EFFECTIVE_VERSION" = "$_major_version.$_minor_version" -a -z "$_micro_version"; then
      # Allow prereleases that have the right effective version.
      true
    else
      as_fn_error $? "Guile $_guile_required_version required, but $_guile_prog_version found" "$LINENO" 5
    fi
  elif test "$GUILE_EFFECTIVE_VERSION" = "$_major_version.$_minor_version" -a -z "$_micro_version"; then
    # Allow prereleases that have the right effective version.
    true
  else
    AC_MSG_ERROR([Guile $_guile_required_version required, but $_guile_prog_version found])
  fi
  AC_MSG_RESULT([$_guile_prog_version])

  AC_PATH_PROGS(GUILD,[guild$_guile_suffix guild])
  AC_SUBST(GUILD)

  AC_PATH_PROGS(GUILE_CONFIG,[guile-config$_guile_suffix guile-config])
  AC_SUBST(GUILE_CONFIG)
  if test -n "$GUILD"; then
    GUILE_TOOLS=$GUILD
  else
    AC_PATH_PROGS(GUILE_TOOLS,[guile-tools$_guile_suffix guile-tools])
  fi
  AC_SUBST(GUILE_TOOLS)
 ])

# GUILE_CHECK -- evaluate Guile Scheme code and capture the return value
#
# Usage: GUILE_CHECK_RETVAL(var,check)
#
# @var{var} is a shell variable name to be set to the return value.
# @var{check} is a Guile Scheme expression, evaluated with "$GUILE -c", and
#    returning either 0 or non-#f to indicate the check passed.
#    Non-0 number or #f indicates failure.
#    Avoid using the character "#" since that confuses autoconf.
#
AC_DEFUN([GUILE_CHECK],
 [AC_REQUIRE([GUILE_PROGS])
  $GUILE -c "$2" > /dev/null 2>&1
  $1=$?
 ])

# GUILE_MODULE_CHECK -- check feature of a Guile Scheme module
#
# Usage: GUILE_MODULE_CHECK(var,module,featuretest,description)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (ice-9 common-list).
# @var{featuretest} is an expression acceptable to GUILE_CHECK, q.v.
# @var{description} is a present-tense verb phrase (passed to AC_MSG_CHECKING).
#
AC_DEFUN([GUILE_MODULE_CHECK],
         [AC_MSG_CHECKING([if $2 $4])
	  GUILE_CHECK($1,(use-modules $2) (exit ((lambda () $3))))
	  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])

# GUILE_MODULE_AVAILABLE -- check availability of a Guile Scheme module
#
# Usage: GUILE_MODULE_AVAILABLE(var,module)
#
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (ice-9 common-list).
#
AC_DEFUN([GUILE_MODULE_AVAILABLE],
         [GUILE_MODULE_CHECK($1,$2,0,is available)
         ])

# GUILE_MODULE_REQUIRED -- fail if a Guile Scheme module is unavailable
#
# Usage: GUILE_MODULE_REQUIRED(symlist)
#
# @var{symlist} is a list of symbols, WITHOUT surrounding parens,
# like: ice-9 common-list.
#
AC_DEFUN([GUILE_MODULE_REQUIRED],
         [GUILE_MODULE_AVAILABLE(ac_guile_module_required, ($1))
          if test "$ac_guile_module_required" = "no" ; then
              AC_MSG_ERROR([required guile module not found: ($1)])
          fi
         ])

# GUILE_MODULE_EXPORTS -- check if a module exports a variable
#
# Usage: GUILE_MODULE_EXPORTS(var,module,modvar)
#
# @var{var} is a shell variable to be set to "yes" or "no".
# @var{module} is a list of symbols, like: (ice-9 common-list).
# @var{modvar} is the Guile Scheme variable to check.
#
AC_DEFUN([GUILE_MODULE_EXPORTS],
 [GUILE_MODULE_CHECK($1,$2,$3,exports `$3')
 ])

# GUILE_MODULE_REQUIRED_EXPORT -- fail if a module doesn't export a variable
#
# Usage: GUILE_MODULE_REQUIRED_EXPORT(module,modvar)
#
# @var{module} is a list of symbols, like: (ice-9 common-list).
# @var{modvar} is the Guile Scheme variable to check.
#
AC_DEFUN([GUILE_MODULE_REQUIRED_EXPORT],
 [GUILE_MODULE_EXPORTS(guile_module_required_export,$1,$2)
  if test "$guile_module_required_export" = "no" ; then
      AC_MSG_ERROR([module $1 does not export $2; required])
  fi
 ])

## guile.m4 ends here
