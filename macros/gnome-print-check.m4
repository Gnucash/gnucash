# Configure paths for GNOME-PRINT
# Chris Lahey	99-2-5
# stolen from Manish Singh again
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_GNOME_PRINT([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GNOME-PRINT, and define GNOME_PRINT_CFLAGS and GNOME_PRINT_LIBS
dnl
AC_DEFUN(AM_PATH_GNOME_PRINT,
[dnl 
dnl Get the cflags and libraries from the gnome-config script
dnl
AC_ARG_WITH(gnome-print-prefix,[  --with-gnome-print-prefix=PFX   Prefix where GNOME-PRINT is installed (optional)],
            gnome_print_prefix="$withval", gnome_print_prefix="")
AC_ARG_WITH(gnome-print-exec-prefix,[  --with-gnome-print-exec-prefix=PFX Exec prefix where GNOME-PRINT is installed (optional)],
            gnome_print_exec_prefix="$withval", gnome_print_exec_prefix="")
AC_ARG_ENABLE(gnome-printtest, [  --disable-gnome-printtest       Do not try to compile and run a test GNOME-PRINT program],
		    , enable_gnome_printtest=yes)

  if test x$gnome_print_exec_prefix != x ; then
     gnome_print_args="$gnome_print_args --exec-prefix=$gnome_print_exec_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$gnome_print_exec_prefix/bin/gnome-config
     fi
  fi
  if test x$gnome_print_prefix != x ; then
     gnome_print_args="$gnome_print_args --prefix=$gnome_print_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$gnome_print_prefix/bin/gnome-config
     fi
  fi

  AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
  min_gnome_print_version=ifelse([$1], ,0.1.0,$1)
  AC_MSG_CHECKING(for GNOME-PRINT - version >= $min_gnome_print_version)
  no_gnome_print=""
  if test "$GNOME_CONFIG" = "no" ; then
    no_gnome_print=yes
  else
    GNOME_PRINT_CFLAGS=`$GNOME_CONFIG $gnome_printconf_args --cflags print`
    GNOME_PRINT_LIBS=`$GNOME_CONFIG $gnome_printconf_args --libs print`

    gnome_print_major_version=`$GNOME_CONFIG $gnome_print_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gnome_print_minor_version=`$GNOME_CONFIG $gnome_print_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gnome_print_micro_version=`$GNOME_CONFIG $gnome_print_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gnome_printtest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GNOME_PRINT_CFLAGS"
      LIBS="$LIBS $GNOME_PRINT_LIBS"
dnl
dnl Now check if the installed GNOME-PRINT is sufficiently new. (Also sanity
dnl checks the results of gnome-config to some extent
dnl
      rm -f conf.gnome_printtest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgnomeprint/gnome-print.h>

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

  system ("touch conf.gnome_printtest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = my_strdup("$min_gnome_print_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gnome_print_version");
     exit(1);
   }
  return 0;
#if 0
   if (($gnome_print_major_version > major) ||
      (($gnome_print_major_version == major) && ($gnome_print_minor_version > minor)) ||
      (($gnome_print_major_version == major) && ($gnome_print_minor_version == minor) && ($gnome_print_micro_version >= micro)))
    {
      return 0;
    }
  else
    {
      printf("\n*** 'gnome-config print --version' returned %d.%d.%d, but the minimum version\n", $gnome_print_major_version, $gnome_print_minor_version, $gnome_print_micro_version);
      printf("*** of GNOME-PRINT required is %d.%d.%d. If gnome-config is correct, then it is\n", major, minor, micro);
      printf("*** best to upgrade to the required version.\n");
      printf("*** If gnome-config was wrong, set the environment variable GNOME_CONFIG\n");
      printf("*** to point to the correct copy of gnome-config, and remove the file\n");
      printf("*** config.cache before re-running configure\n");
      return 1;
    }
#endif
}

],, no_gnome_print=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gnome_print" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GNOME_CONFIG" = "no" ; then
       echo "*** The gnome-config script installed by GNOME-LIBS could not be found"
       echo "*** If GNOME-PRINT was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GNOME_CONFIG environment variable to the"
       echo "*** full path to gnome-config."
     else
       if test -f conf.gnome_printtest ; then
        :
       else
          echo "*** Could not run GNOME-PRINT test program, checking why..."
          CFLAGS="$CFLAGS $GNOME_PRINT_CFLAGS"
          LIBS="$LIBS $GNOME_PRINT_LIBS"
          AC_TRY_LINK([
#include <stdio.h>
#include <libgnomeprint/gnome-print.h>
],      [ return 0; ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GNOME-PRINT or finding the wrong"
          echo "*** version of GNOME-PRINT. If it is not finding GNOME-PRINT, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GNOME-PRINT was incorrectly installed"
          echo "*** or that you have moved GNOME-PRINT since it was installed. In the latter case, you"
          echo "*** may want to edit the gnome-config script: $GNOME_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GNOME_PRINT_CFLAGS=""
     GNOME_PRINT_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GNOME_PRINT_CFLAGS)
  AC_SUBST(GNOME_PRINT_LIBS)
  rm -f conf.gnome_printtest
])

AC_DEFUN([GNOME_PRINT_CHECK], [
	AM_PATH_GNOME_PRINT(0.1.0,,[AC_MSG_ERROR(GNOME-PRINT not found)])
])
