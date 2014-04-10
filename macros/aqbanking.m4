# Id: aqbanking.m4,v 1.3 2005/07/17 13:02:52 cstim Exp
# (c) 2004 Martin Preuss<martin@libchipcard.de>
# This function checks for AqBanking

AC_DEFUN([AC_AQBANKING], [
dnl searches for aqbanking
dnl Arguments: 
dnl   $1: major version minimum
dnl   $2: minor version minimum
dnl   $3: patchlevel version minimum
dnl   $4: build version minimum
dnl Returns: aqbanking_dir
dnl          aqbanking_libs
dnl          aqbanking_libspp
dnl          aqbanking_data
dnl          aqbanking_plugins
dnl          aqbanking_includes
dnl          have_aqbanking

if test -z "$1"; then vma="0"; else vma="$1"; fi
if test -z "$2"; then vmi="1"; else vmi="$2"; fi
if test -z "$3"; then vpl="0"; else vpl="$3"; fi
if test -z "$4"; then vbld="0"; else vbld="$4"; fi

AC_MSG_CHECKING(if aqbanking support desired)
dnl Disabled this switch because it is confusing (as if there were an option)
#AC_ARG_ENABLE(aqbanking,
#  [  --enable-aqbanking      enable aqbanking support (default=yes)],
#  enable_aqbanking="$enableval",
enable_aqbanking="yes"
#)
AC_MSG_RESULT($enable_aqbanking)

have_aqbanking="no"
aqbanking_dir=""
aqbanking_data=""
aqbanking_plugins=""
aqbanking_libs=""
aqbanking_libspp=""
aqbanking_includes=""
if test "$enable_aqbanking" != "no"; then
  AC_MSG_CHECKING(for aqbanking)
  AC_ARG_WITH(aqbanking-dir, [  --with-aqbanking-dir=DIR
                            uses aqbanking from given dir],
    [lcc_dir="$withval"],
    [lcc_dir="${prefix} \
	     /usr/local \
             /usr \
             /"])

  for li in $lcc_dir; do
      if test -x "$li/bin/aqbanking-config"; then
          aqbanking_dir="$li";
          break
      fi
  done
  if test -z "$aqbanking_dir"; then
      AC_MSG_RESULT([not found ])
      AC_MSG_ERROR([
*** The library AqBanking was not found. Obtain it from 
*** http://www.aquamaniac.de. 
*** If it is already installed (including the -devel package), 
*** you might need to specify the location with the 
*** option --with-aqbanking-dir=DIR.])
  else
      AC_MSG_RESULT($aqbanking_dir)
      AC_MSG_CHECKING(for aqbanking libs)
      aqbanking_libs="`$aqbanking_dir/bin/aqbanking-config --libraries`"
      AC_MSG_RESULT($aqbanking_libs)
      AC_MSG_CHECKING(for aqbanking C++ libs)
      aqbanking_libspp="`$aqbanking_dir/bin/aqbanking-config --libraries++`"
      AC_MSG_RESULT($aqbanking_libspp)
      AC_MSG_CHECKING(for aqbanking includes)
      aqbanking_includes="`$aqbanking_dir/bin/aqbanking-config --includes`"
      AC_MSG_RESULT($aqbanking_includes)
      AC_MSG_CHECKING(for aqbanking plugins)
      aqbanking_plugins="`$aqbanking_dir/bin/aqbanking-config --plugins`"
      AC_MSG_RESULT($aqbanking_plugins)
      AC_MSG_CHECKING(for aqbanking data)
      aqbanking_data="`$aqbanking_dir/bin/aqbanking-config --data`"
      AC_MSG_RESULT($aqbanking_data)
  fi
  AC_MSG_CHECKING(if aqbanking test desired)
  AC_ARG_ENABLE(aqbanking,
    [  --enable-aqbanking-test   enable aqbanking-test (default=yes)],
     enable_aqbanking_test="$enableval",
     enable_aqbanking_test="yes")
  AC_MSG_RESULT($enable_aqbanking_test)
  AC_MSG_CHECKING(for AqBanking version >=$vma.$vmi.$vpl.$vbld)
  if test "$enable_aqbanking_test" != "no"; then
    aqbanking_versionstring="`$aqbanking_dir/bin/aqbanking-config --vstring`.`$aqbanking_dir/bin/aqbanking-config --vbuild`"
    AC_MSG_RESULT([found $aqbanking_versionstring])
    if test "$vma" -gt "`$aqbanking_dir/bin/aqbanking-config --vmajor`"; then
      AC_MSG_ERROR([Your AqBanking version is way too old.
      Please update from http://www.aquamaniac.de/aqbanking/])
    elif test "$vma" = "`$aqbanking_dir/bin/aqbanking-config --vmajor`"; then
      if test "$vmi" -gt "`$aqbanking_dir/bin/aqbanking-config --vminor`"; then
        AC_MSG_ERROR([Your AqBanking version is too old.
          Please update from http://www.aquamaniac.de/aqbanking/])
      elif test "$vmi" = "`$aqbanking_dir/bin/aqbanking-config --vminor`"; then
          if test "$vpl" -gt "`$aqbanking_dir/bin/aqbanking-config --vpatchlevel`"; then
            AC_MSG_ERROR([Your AqBanking version is a little bit too old.
            Please update from http://www.aquamaniac.de/aqbanking/])
          elif test "$vpl" = "`$aqbanking_dir/bin/aqbanking-config --vpatchlevel`"; then
            if test "$vbld" -gt "`$aqbanking_dir/bin/aqbanking-config --vbuild`"; then
              AC_MSG_ERROR([Your AqBanking version is a little bit too old. 
  Please update to the latest CVS version. Instructions for accessing 
  CVS can be found on http://www.aquamaniac.de/aqbanking/])
             fi
           fi
      fi
    fi
    have_aqbanking="yes"
    #AC_MSG_RESULT(yes)
  else
    have_aqbanking="yes"
    AC_MSG_RESULT(assuming yes)
  fi
dnl end of "if enable-aqbanking"
fi

AC_SUBST(aqbanking_dir)
AC_SUBST(aqbanking_plugins)
AC_SUBST(aqbanking_libs)
AC_SUBST(aqbanking_libspp)
AC_SUBST(aqbanking_data)
AC_SUBST(aqbanking_includes)
])
