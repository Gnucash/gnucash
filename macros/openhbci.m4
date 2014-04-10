# Configure paths for OpenHBCI
# by Christian Stimming 2002-07-30
# Copied from glib-2.0.m4 by Owen Taylor     1997-2001

dnl AM_PATH_OPENHBCI([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for OPENHBCI, and define OPENHBCI_CFLAGS, OPENHBCI_CXXFLAGS and OPENHBCI_LIBS
dnl
AC_DEFUN(AM_PATH_OPENHBCI,
[dnl 
dnl Get the c[xx]flags and libraries from openhbci-config
dnl
AC_MSG_CHECKING(for OpenHBCI)
AC_ARG_ENABLE(openhbcitest, [  --disable-openhbcitest      do not try to compile and run a test OpenHBCI program],
		    , enable_openhbcitest=yes)

AC_ARG_WITH( openhbci-prefix,
  [  --with-openhbci-prefix=PFX   Prefix where OpenHBCI is installed],
  OPENHBCI_PREFIX="$with_openhbci_prefix",
  OPENHBCI_PREFIX="${prefix} \
		/usr/local \
		/usr/local/openhbci \
		/usr")

hbci_config=""
for li in $OPENHBCI_PREFIX; do
  if test -x "${li}/bin/openhbci-config"; then
    hbci_config="${li}/bin/openhbci-config"
    hbci_dir="${li}"
    break
  fi
done

if test -x "${hbci_config}" ; then
  OPENHBCI_LIBS="`${hbci_config} --libraries`"
  OPENHBCI_CFLAGS="`${hbci_config} --includes`"
  OPENHBCI_CXXFLAGS="`${hbci_config} --includes`"
  AS_SCRUB_INCLUDE(OPENHBCI_CFLAGS)
  AS_SCRUB_INCLUDE(OPENHBCI_CXXFLAGS)
else
  AC_MSG_ERROR([
  Could not find bin/openhbci-config in prefix ${OPENHBCI_PREFIX}.
  Please specify the right path by --with-openhbci-prefix=PREFIX.
  ])
fi  
AC_MSG_RESULT($hbci_dir)

min_openhbci_version=ifelse([$1], ,0.9.0,$1)
AC_MSG_CHECKING(for OpenHBCI - version >= $min_openhbci_version)

if test "x$enable_openhbcitest" = "xyes" ; then
  ac_save_CFLAGS="$CFLAGS"
  ac_save_LIBS="$LIBS"
  CFLAGS="$CFLAGS $OPENHBCI_CFLAGS"
  LIBS="$OPENHBCI_LIBS $LIBS"
dnl
dnl Now check if the installed OpenHBCI is sufficiently new. (Also sanity
dnl checks the results of path guessing to some extent)
dnl
  rm -f conf.openhbcitest
  AC_TRY_RUN([
#include <openhbci.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int 
main ()
{
  int major, minor, micro;
  int getmajor, getminor, getmicro;
  char *tmp_version;

  system ("touch conf.openhbcitest");

  /* HP/UX 9 writes to sscanf strings */
  tmp_version = strdup("$min_openhbci_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
    printf("%s, bad version string\n", "$min_openhbci_version");
    exit(1);
  }

  HBCI_Hbci_libraryVersion(&getmajor, &getminor, &getmicro);

  if ((getmajor != OPENHBCI_VERSION_MAJOR) ||
      (getminor != OPENHBCI_VERSION_MINOR) ||
      (getmicro != OPENHBCI_VERSION_PATCHLEVEL))
    {
      printf("*** OpenHBCI header files (version %d.%d.%d) do not match\n",
	     OPENHBCI_VERSION_MAJOR, 
	     OPENHBCI_VERSION_MINOR, 
	     OPENHBCI_VERSION_PATCHLEVEL);
      printf("*** installed library (version %d.%d.%d)\n",
	     getmajor, getminor, getmicro);
      exit(1);
    }
  else
    {
      if ((getmajor > major) ||
	  ((getmajor == major) && (getminor > minor)) ||
	  ((getmajor == major) && (getminor == minor) && (getmicro >= micro)))
	{
	  return 0;
	}
      else
	{
	  printf("\n*** An old version of OpenHBCI (%d.%d.%d) was found.\n",
		 getmajor, getminor, getmicro);
	  printf("*** You need OpenHBCI in version %d.%d.%d or newer. The latest version\n",
		 major, minor, micro);
	  printf("*** of OpenHBCI is always available from http://www.openhbci.de.\n");
	  printf("***\n");
	  printf("*** If you have already installed a sufficiently new version, this error\n");
	  printf("*** probably means that you need to specify the right path by\n");
	  printf("*** --with-openhbci-prefix=PATH.  (Also, check that your LD_LIBRARY_PATH\n");
	  printf("*** enviroment variable is correct, or edit /etc/ld.so.conf\n");
	  printf("*** so that the correct libraries are found at run-time))\n");
	}
    }
  return 1;
}
],, no_openhbci=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
  CFLAGS="$ac_save_CFLAGS"
  LIBS="$ac_save_LIBS"
fi
if test "x$no_openhbci" = x ; then
  AC_MSG_RESULT(yes)
  ifelse([$2], , :, [$2])     
else
  AC_MSG_RESULT(no)
  if test -f conf.openhbcitest ; then
   :
  else
    echo "*** Could not run OpenHBCI test program, checking why..."
    ac_save_CFLAGS="$CFLAGS"
    ac_save_LIBS="$LIBS"
    CFLAGS="$CFLAGS $OPENHBCI_CFLAGS"
    LIBS="$LIBS $OPENHBCI_LIBS"
    AC_TRY_LINK([
#include <openhbci.h>
#include <stdio.h>
],      [ return ((OPENHBCI_VERSION_MAJOR) || (OPENHBCI_VERSION_MINOR) || (OPENHBCI_VERSION_PATCHLEVEL)); ],
        [ echo "***"
	  echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding OpenHBCI or finding the wrong"
          echo "*** version of OpenHBCI. If it is not finding OpenHBCI, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system."
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH." 
	  echo "***" ],
        [ echo "***"
	  echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means OpenHBCI is incorrectly installed."])
    CFLAGS="$ac_save_CFLAGS"
    LIBS="$ac_save_LIBS"
  fi
  OPENHBCI_CFLAGS=""
  OPENHBCI_LIBS=""
  ifelse([$3], , :, [$3])
  exit 1;
fi
AC_SUBST(OPENHBCI_CFLAGS)
AC_SUBST(OPENHBCI_LIBS)
AC_SUBST(OPENHBCI_CXXFLAGS)
rm -f conf.openhbcitest
])
