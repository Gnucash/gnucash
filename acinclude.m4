# aclocal-include.m4
# 
# This macro adds the name macrodir to the set of directories
# that `aclocal' searches for macros.  

# serial 1

dnl AM_ACLOCAL_INCLUDE(macrodir)
AC_DEFUN([AM_ACLOCAL_INCLUDE],
[
	AM_CONDITIONAL(INSIDE_GNOME_COMMON, test x = y)

	test -n "$ACLOCAL_FLAGS" && ACLOCAL="$ACLOCAL $ACLOCAL_FLAGS"

	for k in $1 ; do ACLOCAL="$ACLOCAL -I $k" ; done
])

dnl g-wrap.m4
dnl Written by Robert Merkel <rgmerk@mira.net>
dnl Parts ripped off from guile.m4 and ORBit.m4

dnl check whether we use the old or new guile smobs

AC_DEFUN(AC_GWRAP_CHECK_GUILE,
[if test x$GUILE = x ; then
	AC_PATH_PROG(GUILE, guile, no)
 fi
 dnl AC_MSG_WARN(guile is $GUILE)
 if test "${GUILE}" = "no" ; then
	AC_MSG_ERROR(g-wrap couldn't find guile.)
 fi
])

dnl AM_PATH_GWRAP ([MINIMUM-VERSION, [ACTION-IF-FOUND.
dnl	           [ACTION-IF-NOT-FOUND]]])

dnl tests for minimum version of g-wrap.
dnl sets G_WRAP_CONFIG and GWRAP_OLD_GUILE_SMOB if needed.

AC_DEFUN(AM_PATH_GWRAP,
[dnl
dnl
dnl
AC_ARG_WITH(g-wrap-prefix,[ --with-g-wrap-prefix=PFX  Prefix where g-wrap is installed (optional)], 
    gwrap_prefix="$withval", g_wrap_prefix="")

min_gwrap_version=ifelse([$1], , 0.9.1,$1)

if test x${GUILE} = x ; then
   AC_PATH_PROG(GUILE, guile, no)
fi

dnl if prefix set, then set them explicitly
if test x${gwrap_prefix} != x ; then
   G_WRAP_CONFIG=${gwrap_prefix}/bin/g-wrap-config
else

  AC_PATH_PROG(G_WRAP_CONFIG, g-wrap-config, no)
  if test x${G_WRAP_CONFIG} = xno ; then
	CHECK_VERSION="no"
	ifelse([$3], , true , [AC_MSG_WARN(g-wrap-config failed)
	$3])
  fi
fi

if test x$CHECK_VERSION != xno ; then
AC_MSG_CHECKING(for g-wrap - version >= ${min_gwrap_version})

gwrap_major_version=`${G_WRAP_CONFIG} --version | \
	sed 's/g-wrap-config \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
gwrap_minor_version=`${G_WRAP_CONFIG} --version | \
	sed 's/g-wrap-config \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
gwrap_micro_version=`${G_WRAP_CONFIG} --version | \
	sed 's/g-wrap-config \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`


major_required=`echo ${min_gwrap_version} |\
        sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
minor_required=`echo ${min_gwrap_version} |\
	sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
micro_required=`echo ${min_gwrap_version} |\
	sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

if ${GUILE} -c "(cond ((> ${gwrap_major_version} ${major_required}) (exit 0))\
	           ((< ${gwrap_major_version} ${major_required}) (exit 1))\
                   ((> ${gwrap_minor_version} ${minor_required}) (exit 0))\
		   ((< ${gwrap_minor_version} ${minor_required}) (exit 1))\
	           ((< ${gwrap_micro_version} ${micro_required}) (exit 1))\
		   (else (exit 0)))" ; then
	AC_MSG_RESULT(yes)
	ifelse([$2], , true, [$2])
else
	AC_MSG_RESULT(no)
	ifelse([$3], , true , [AC_MSG_WARN(guile check failed)
	$3])
fi
dnl check version
fi])


dnl ##############################

# Configure paths for libguppi
# stolen without shame from Chris Lahey
# stolen from Manish Singh again
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_LIBGUPPI([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for LIBGUPPI, and define LIBGUPPI_CFLAGS and LIBGUPPI_LIBS
dnl
AC_DEFUN(AM_PATH_LIBGUPPI,
[dnl 
dnl Get the cflags and libraries from the gnome-config script
dnl
AC_ARG_WITH(libguppi-prefix,[  --with-libguppi-prefix=PFX   Prefix where libguppi is installed (optional)],
            libguppi_prefix="$withval", libguppi_prefix="")
AC_ARG_WITH(libguppi-exec-prefix,[  --with-libguppi-exec-prefix=PFX Exec prefix where libguppi is installed (optional)],
            libguppi_exec_prefix="$withval", libguppi_exec_prefix="")
AC_ARG_ENABLE(libguppitest, [  --disable-libguppitest       Do not try to compile and run a test libguppi program],
		    , enable_libguppitest=yes)

  if test x$libguppi_exec_prefix != x ; then
     libguppi_args="$libguppi_args --exec-prefix=$libguppi_exec_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$libguppi_exec_prefix/bin/gnome-config
     fi
  fi
  if test x$libguppi_prefix != x ; then
     libguppi_args="$libguppi_args --prefix=$libguppi_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$libguppi_prefix/bin/gnome-config
     fi
  fi

  AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
  min_libguppi_version=ifelse([$1], ,0.1.0,$1)
  AC_MSG_CHECKING(for libguppi - version >= $min_libguppi_version)
  no_libguppi=""
  if test "$GNOME_CONFIG" = "no" ; then
    no_libguppi=yes
  else
    LIBGUPPI_CFLAGS=`$GNOME_CONFIG $libguppiconf_args --cflags gtk libguppi`
    LIBGUPPI_LIBS=`$GNOME_CONFIG $libguppiconf_args --libs gtk libguppi`

    libguppi_major_version=`$GNOME_CONFIG --modversion libguppi | \
           sed 's/libguppi-\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    libguppi_minor_version=`$GNOME_CONFIG --modversion libguppi | \
           sed 's/libguppi-\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    libguppi_micro_version=`$GNOME_CONFIG --modversion libguppi | \
           sed 's/libguppi-\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

    if test "x$enable_libguppitest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $LIBGUPPI_CFLAGS"
      LIBS="$LIBS $LIBGUPPI_LIBS"
dnl
dnl Now check if the installed LIBGUPPI is sufficiently new. (Also sanity
dnl checks the results of gnome-config to some extent
dnl
      rm -f conf.libguppitest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libguppi/guppi-version.h>

static char*
my_strdup (char *str)
{
  char *new_str;
  
  if (str)
    {
      new_str = malloc ((strlen (str) + 1) * sizeof(char));
      strcpy (new_str, str);
    }
  else
    new_str = NULL;
  
  return new_str;
}

int main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.libguppitest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = my_strdup("$min_libguppi_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_libguppi_version");
     exit(1);
   }

   if (($libguppi_major_version != guppi_version_major()) ||
       ($libguppi_minor_version != guppi_version_minor()) ||
       ($libguppi_micro_version != guppi_version_micro())) {
	printf("\n*** 'gnome-config --modversion libguppi' returned %d.%d.%d, but the installed\n", $libguppi_major_version, $libguppi_minor_version, $libguppi_micro_version);
	printf("*** LIBGUPPI reports its own version as %s.\n\n", guppi_version());
	printf("*** This probably means that LIBGUPPI is improperly installed.\n");
	return 1;
   }	

   if (($libguppi_major_version > major) ||
      (($libguppi_major_version == major) && ($libguppi_minor_version > minor)) ||
      (($libguppi_major_version == major) && ($libguppi_minor_version == minor) && ($libguppi_micro_version >= micro)))
    {
      return 0;
    }
  else
    {
      printf("\n*** 'gnome-config --modversion libguppi' returned %d.%d.%d, but the minimum version\n", $libguppi_major_version, $libguppi_minor_version, $libguppi_micro_version);
      printf("*** of libguppi required is %d.%d.%d. If gnome-config is correct, then it is\n", major, minor, micro);
      printf("*** best to upgrade to the required version.\n");
      printf("*** If gnome-config was wrong, set the environment variable GNOME_CONFIG\n");
      printf("*** to point to the correct copy of gnome-config, and remove the file\n");
      printf("*** config.cache before re-running configure\n");
      return 1;
    }
}

],, no_libguppi=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_libguppi" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GNOME_CONFIG" = "no" ; then
       echo "*** The gnome-config script installed by GNOME-LIBS could not be found"
       echo "*** If libguppi was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GNOME_CONFIG environment variable to the"
       echo "*** full path to gnome-config."
     else
       if test -f conf.libguppitest ; then
        :
       else
          echo "*** Could not run libguppi test program, checking why..."
          CFLAGS="$CFLAGS $LIBGUPPI_CFLAGS"
          LIBS="$LIBS $LIBGUPPI_LIBS"
          AC_TRY_LINK([
#include <stdio.h>
#include <libguppi/guppi-useful.h>
],      [ return 0; ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding libguppi or finding the wrong"
          echo "*** version of libguppi. If it is not finding libguppi, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means libguppi was incorrectly installed"
          echo "*** or that you have moved libguppi since it was installed. In the latter case, you"
          echo "*** may want to edit the gnome-config script: $GNOME_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     LIBGUPPI_CFLAGS=""
     LIBGUPPI_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(LIBGUPPI_CFLAGS)
  AC_SUBST(LIBGUPPI_LIBS)
  rm -f conf.libguppitest
])

AC_DEFUN([LIBGUPPI_CHECK], [
	AM_PATH_LIBGUPPI(0.35.1,,[AC_MSG_ERROR(libguppi not found)])
])

AC_DEFUN([LANGINFO_D_FMT_CHECK],
[
  AC_CACHE_CHECK([for nl_langinfo and D_FMT], am_cv_langinfo_dfmt,
    [AC_TRY_LINK([#include <langinfo.h>],
      [char* cs = nl_langinfo(D_FMT);],
      am_cv_langinfo_dfmt=yes,
      am_cv_langinfo_dfmt=no)
    ])
  if test $am_cv_langinfo_dfmt = yes; then
    AC_DEFINE(HAVE_LANGINFO_D_FMT, 1,
      [Define if you have <langinfo.h> and nl_langinfo(D_FMT).])
  fi
])

AC_DEFUN([STRUCT_TM_GMTOFF_CHECK],
[
  AC_CACHE_CHECK([for the tm_gmtoff member of struct tm],
                 am_cv_struct_tm_gmtoff,
    [AC_TRY_LINK([
        #include <time.h>
        #define _GNU_SOURCE
        #define __EXTENSIONS__
],
      [struct tm tm;
       tm.tm_gmtoff = 0;],
      am_cv_struct_tm_gmtoff=yes,
      am_cv_struct_tm_gmtoff=no)
    ])
  if test $am_cv_struct_tm_gmtoff = yes; then
    AC_DEFINE(HAVE_STRUCT_TM_GMTOFF, 1,
      [Define if you have the tm_gmtoff member of struct tm.])
  fi
])

AC_DEFUN([SCANF_LLD_CHECK],
[
  AC_CACHE_CHECK([if scanf supports %lld conversions],
                 am_cv_scanf_lld,
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>

int main ()
{
  long long int d;

  d = 0;
  if ((sscanf ("10000000000", "%lld", &d) != 1) || (d != 10000000000))
    exit (1);

  exit (0);
}
],
        am_cv_scanf_lld=yes,
        am_cv_scanf_lld=no))
  if test $am_cv_scanf_lld = yes; then
    AC_DEFINE(HAVE_SCANF_LLD, 1,
      [Define if scanf supports %lld conversions.])
  fi
])
