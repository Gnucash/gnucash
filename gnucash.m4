# Configure paths for GnuCash
# Dave Peticolas <dave@krondo.com> 2001-09-07
# Based on glib.m4 by Owen Taylor 97-11-3

dnl AM_PATH_GNUCASH([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GnuCash, and define GNUCASH_CFLAGS and GNUCASH_LIBS, if "gmodule" or 
dnl gthread is specified in MODULES, pass to gnucash-config
dnl
AC_DEFUN(AM_PATH_GNUCASH,
[dnl 
dnl Get the cflags and libraries from the gnucash-config script
dnl
AC_ARG_WITH(gnucash-prefix,[  --with-gnucash-prefix=PFX   Prefix where GnuCash is installed (optional)],
            gnucash_config_prefix="$withval", gnucash_config_prefix="")
AC_ARG_WITH(gnucash-exec-prefix,[  --with-gnucash-exec-prefix=PFX Exec prefix where GnuCash is installed (optional)],
            gnucash_config_exec_prefix="$withval", gnucash_config_exec_prefix="")
AC_ARG_ENABLE(gnucashtest, [  --disable-gnucashtest       Do not try to compile and run a test GnuCash program],
		    , enable_gnucashtest=yes)

  if test x$gnucash_config_exec_prefix != x ; then
     gnucash_config_args="$gnucash_config_args --exec-prefix=$gnucash_config_exec_prefix"
     if test x${GNUCASH_CONFIG+set} != xset ; then
        GNUCASH_CONFIG=$gnucash_config_exec_prefix/bin/gnucash-config
     fi
     if test x${GNUCASH_RUN_SCRIPT+set} != xset ; then
        GNUCASH_RUN_SCRIPT=$gnucash_config_exec_prefix/bin/gnucash-run-script
     fi
  fi
  if test x$gnucash_config_prefix != x ; then
     gnucash_config_args="$gnucash_config_args --prefix=$gnucash_config_prefix"
     if test x${GNUCASH_CONFIG+set} != xset ; then
        GNUCASH_CONFIG=$gnucash_config_prefix/bin/gnucash-config
     fi
     if test x${GNUCASH_RUN_SCRIPT+set} != xset ; then
        GNUCASH_RUN_SCRIPT=$gnucash_config_prefix/bin/gnucash-run-script
     fi
  fi

  if test "x$4" = "x"; then
     gnucash_config_args="$gnucash_config_args gnucash"
  else
     gnucash_config_args="$gnucash_config_args $4"
  fi

  AC_PATH_PROG(GNUCASH_CONFIG, gnucash-config, no)
  min_gnucash_version=ifelse([$1], ,1.7.0,$1)
  AC_MSG_CHECKING(for GnuCash - version >= $min_gnucash_version)
  no_gnucash=""
  if test "$GNUCASH_CONFIG" = "no" ; then
    no_gnucash=yes
  else
    GNUCASH_CFLAGS=`$GNUCASH_CONFIG $gnucash_config_args --cflags`
    GNUCASH_LIBS=`$GNUCASH_CONFIG $gnucash_config_args --libs`
    gnucash_config_major_version=`$GNUCASH_CONFIG $gnucash_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gnucash_config_minor_version=`$GNUCASH_CONFIG $gnucash_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gnucash_config_micro_version=`$GNUCASH_CONFIG $gnucash_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gnucashtest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GNUCASH_CFLAGS"
      LIBS="$GNUCASH_LIBS $LIBS"
dnl
dnl Now check if the installed GNUCASH is sufficiently new. (Also sanity
dnl checks the results of gnucash-config to some extent
dnl
      rm -f conf.gnucashtest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>

#include <gnc-engine.h>
#include <Group.h>

int
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gnucashtest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gnucash_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gnucash_version");
     exit(1);
  }

  if ((gnucash_major_version() != $gnucash_config_major_version) ||
      (gnucash_minor_version() != $gnucash_config_minor_version) ||
      (gnucash_micro_version() != $gnucash_config_micro_version))
    {
      printf("\n*** 'gnucash-config --version' returned %d.%d.%d, but GnuCash (%d.%d.%d)\n", 
             $gnucash_config_major_version, $gnucash_config_minor_version, $gnucash_config_micro_version,
             gnucash_major_version(), gnucash_minor_version(), gnucash_micro_version());
      printf ("*** was found! If gnucash-config was correct, then it is best\n");
      printf ("*** to remove the old version of GnuCash. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
      printf("*** If gnucash-config was wrong, set the environment variable GNUCASH_CONFIG\n");
      printf("*** to point to the correct copy of gnucash-config, and remove the file config.cache\n");
      printf("*** before re-running configure\n");
    }
/* The code below needs GNUCASH_*_VERSION defined in gnc-engine.h
  else if ((gnucash_major_version() != GNUCASH_MAJOR_VERSION) ||
	   (gnucash_minor_version() != GNUCASH_MINOR_VERSION) ||
           (gnucash_micro_version() != GNUCASH_MICRO_VERSION))
    {
      printf("*** GnuCash header files (version %d.%d.%d) do not match\n",
	     GNUCASH_MAJOR_VERSION, GNUCASH_MINOR_VERSION, GNUCASH_MICRO_VERSION);
      printf("*** library (version %d.%d.%d)\n",
	     gnucash_major_version(), gnucash_minor_version(), gnucash_micro_version());
    }
*/
  else
    {
      if ((gnucash_major_version() > major) ||
        ((gnucash_major_version() == major) && (gnucash_minor_version() > minor)) ||
        ((gnucash_major_version() == major) && (gnucash_minor_version() == minor) && (gnucash_micro_version() >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GnuCash (%d.%d.%d) was found.\n",
               gnucash_major_version(), gnucash_minor_version(), gnucash_micro_version());
        printf("*** You need a version of GnuCash newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GnuCash is always available from ftp://ftp.gnucash.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the gnucash-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GnuCash, but you can also set the GNUCASH_CONFIG environment to point to the\n");
        printf("*** correct copy of gnucash-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
      }
    }
  return 1;
}
],, no_gnucash=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gnucash" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GNUCASH_CONFIG" = "no" ; then
       echo "*** The gnucash-config script installed by GnuCash could not be found"
       echo "*** If GnuCash was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GNUCASH_CONFIG environment variable to the"
       echo "*** full path to gnucash-config."
     else
       if test -f conf.gnucashtest ; then
        :
       else
          echo "*** Could not run GnuCash test program, checking why..."
          CFLAGS="$CFLAGS $GNUCASH_CFLAGS"
          LIBS="$LIBS $GNUCASH_LIBS"
          AC_TRY_LINK([
#include <gnc-engine.h>
#include <stdio.h>
],      [ return ((gnucash_major_version()) || (gnucash_minor_version()) || (gnucash_micro_version())); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GnuCash or finding the wrong"
          echo "*** version of GnuCash. If it is not finding GnuCash, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GnuCash was incorrectly installed"
          echo "*** or that you have moved GnuCash since it was installed. In the latter case, you"
          echo "*** may want to edit the gnucash-config script: $GNUCASH_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GNUCASH_CFLAGS=""
     GNUCASH_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GNUCASH_CFLAGS)
  AC_SUBST(GNUCASH_LIBS)
  rm -f conf.gnucashtest
  AC_PATH_PROG(GNUCASH_RUN_SCRIPT, gnucash-run-script, gnucash-run-script)
  AC_SUBST(GNUCASH_RUN_SCRIPT)
])
