# Configure paths for Bonobo
# Miguel de Icaza, 99-04-12
# Stolen from Chris Lahey	99-2-5
# stolen from Manish Singh again
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_BONOBO ([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for Bonobo, and define BONOBO_CFLAGS and BONOBO_LIBS
dnl
AC_DEFUN([AM_PATH_BONOBO],
[
dnl 
dnl Get the cflags and libraries from the gnome-config script
dnl
AC_ARG_WITH(bonobo-prefix,[  --with-bonobo-prefix=PFX   Prefix where Bonobo is installed (optional)],
            bonobo_prefix="$withval", bonobo_prefix="")
AC_ARG_WITH(bonobo-exec-prefix,[  --with-bonobo-exec-prefix=PFX Exec prefix where Bonobo is installed (optional)],
            bonobo_exec_prefix="$withval", bonobo_exec_prefix="")
AC_ARG_ENABLE(bonobotest, [  --disable-bonobotest       Do not try to compile and run a test Bonobo program],
		    , enable_bonobotest=yes)

  if test x$bonobo_exec_prefix != x ; then
     bonobo_args="$bonobo_args --exec-prefix=$bonobo_exec_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$bonobo_exec_prefix/bin/gnome-config
     fi
  fi
  if test x$bonobo_prefix != x ; then
     bonobo_args="$bonobo_args --prefix=$bonobo_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$bonobo_prefix/bin/gnome-config
     fi
  fi

  AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
  min_bonobo_version=ifelse([$1], ,0.1.0,$1)
  AC_MSG_CHECKING(for BONOBO - version >= $min_bonobo_version)
  no_bonobo=""
  if test "$GNOME_CONFIG" = "no" ; then
    no_bonobo=yes
  else
    BONOBO_CFLAGS=`$GNOME_CONFIG $bonoboconf_args --cflags bonobo`
    BONOBO_LIBS=`$GNOME_CONFIG $bonoboconf_args --libs bonobo`

    bonobo_major_version=`$GNOME_CONFIG $bonobo_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    bonobo_minor_version=`$GNOME_CONFIG $bonobo_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    bonobo_micro_version=`$GNOME_CONFIG $bonobo_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_bonobotest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $BONOBO_CFLAGS"
      LIBS="$LIBS $BONOBO_LIBS"
dnl
dnl Now check if the installed BONOBO is sufficiently new. (Also sanity
dnl checks the results of gnome-config to some extent
dnl
      rm -f conf.bonobotest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bonobo.h>

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

  system ("touch conf.bonobotest");
  bonobo_object_get_type ();
  return 0;
}

],, no_bonobo=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_bonobo" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GNOME_CONFIG" = "no" ; then
       echo "*** The gnome-config script installed by GNOME-LIBS could not be found"
       echo "*** If BONOBO was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GNOME_CONFIG environment variable to the"
       echo "*** full path to gnome-config."
     else
       if test -f conf.bonobotest ; then
        :
       else
          echo "*** Could not run BONOBO test program, checking why..."
          CFLAGS="$CFLAGS $BONOBO_CFLAGS"
          LIBS="$LIBS $BONOBO_LIBS"
          AC_TRY_LINK([
#include <stdio.h>
#include <bonobo/gnome-object.h>
],      [ return 0; ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding BONOBO or finding the wrong"
          echo "*** version of BONOBO. If it is not finding BONOBO, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means BONOBO was incorrectly installed"
          echo "*** or that you have moved BONOBO since it was installed. In the latter case, you"
          echo "*** may want to edit the gnome-config script: $GNOME_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     BONOBO_CFLAGS=""
     BONOBO_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(BONOBO_CFLAGS)
  AC_SUBST(BONOBO_LIBS)
  rm -f conf.bonobotest
])

AC_DEFUN([BONOBO_CHECK], [
	AM_PATH_BONOBO(0.1.0,,[AC_MSG_ERROR(BONOBO not found)])
])

AC_DEFUN([AM_BONOBO_USES_OAF],
[
	AC_REQUIRE([AM_PATH_BONOBO])

	AC_MSG_CHECKING(if Bonobo uses OAF)
	if ( gnome-config --libs bonobo | grep oaf ) > /dev/null 2>&1 ; then
	  using_oaf="yes"
	  AC_DEFINE(BONOBO_USES_OAF)
	else
	  using_oaf="no"
	fi

	AC_MSG_RESULT("$using_oaf")

	AM_CONDITIONAL(BONOBO_USES_OAF, test x"using_oaf" = "xyes")
])
