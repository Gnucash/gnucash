# Configure paths for OpenHBCI
# by Christian Stimming 2002-07-30
# Copied from glib-2.0.m4 by Owen Taylor     1997-2001

dnl AM_PATH_OPENHBCI([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for OPENHBCI, and define OPENHBCI_CFLAGS, OPENHBCI_CXXFLAGS and OPENHBCI_LIBS
dnl
AC_DEFUN(AM_PATH_OPENHBCI, [
dnl 
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
else
  AC_MSG_ERROR([
  Could not find bin/openhbci-config in prefix ${OPENHBCI_PREFIX}.
  Please specify the right path by --with-openhbci-prefix=PREFIX.
  ])
fi  
AC_MSG_RESULT($hbci_dir)

min_openhbci_version=ifelse([$1], ,0.9.0.0,$1)
AC_MSG_CHECKING(for OpenHBCI - version >= $min_openhbci_version)
AC_LANG_PUSH(C++)

if test "x$enable_openhbcitest" = "xyes" ; then
  ac_save_CXXFLAGS="$CXXFLAGS"
  ac_save_LIBS="$LIBS"
  CXXFLAGS="$CXXFLAGS $OPENHBCI_CXXFLAGS"
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

#ifndef OPENHBCI_VERSION_BUILD
#  define OPENHBCI_VERSION_BUILD 0
#endif
int 
main ()
{
  int major, minor, micro, build;
  int getmajor, getminor, getmicro, getbuild;
  char *tmp_version;

  system ("touch conf.openhbcitest");

  /* HP/UX 9 writes to sscanf strings */
  tmp_version = strdup("$min_openhbci_version");
  build=0;
  if (sscanf(tmp_version, "%d.%d.%d.%d", &major, &minor, &micro, &build) != 4)
  {
    build = 0;
    if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3)
    {
      micro = 0;
      if (sscanf(tmp_version, "%d.%d", &major, &minor) != 2)
      {
        minor = 0;
        if (sscanf(tmp_version, "%d", &major) != 1)
        {
          printf("\n*** AM PATH OPENHBCI: bad version string: %s\n", "$min_openhbci_version");
          exit(1);
        }
      }
    }
  }

  /* in C: HBCI_Hbci_libraryVersion(&getmajor, &getminor, &getmicro); */
#if ((OPENHBCI_VERSION_MAJOR>0) || (OPENHBCI_VERSION_MINOR>9) || \
      (OPENHBCI_VERSION_PATCHLEVEL>9) || (OPENHBCI_VERSION_BUILD>8))
  /* Four-argument libraryVersion() was introduced with 0.9.9.9. */
  HBCI::Hbci::libraryVersion(getmajor, getminor, getmicro, getbuild);
#else
  HBCI::Hbci::libraryVersion(getmajor, getminor, getmicro);
  getbuild = OPENHBCI_VERSION_BUILD;
#endif

  if ((getmajor != OPENHBCI_VERSION_MAJOR) ||
      (getminor != OPENHBCI_VERSION_MINOR) ||
      (getmicro != OPENHBCI_VERSION_PATCHLEVEL) ||
      (getbuild != OPENHBCI_VERSION_BUILD))
    {
      printf("\n*** OpenHBCI header files openhbci.h "
	     "(version %d.%d.%d build %d) do not match\n",
	     OPENHBCI_VERSION_MAJOR, 
	     OPENHBCI_VERSION_MINOR, 
	     OPENHBCI_VERSION_PATCHLEVEL,
	     OPENHBCI_VERSION_BUILD);
      printf("*** installed library libopenhbci.so "
	     "(version %d.%d.%d build %d)\n",
	     getmajor, getminor, getmicro, getbuild);
      exit(1);
    }
  else
    {
      if ((getmajor > major) ||
	  ((getmajor == major) && (getminor > minor)) ||
	  ((getmajor == major) && (getminor == minor) && (getmicro > micro)) ||
	  ((getmajor == major) && (getminor == minor) && 
	   (getmicro == micro) && (getbuild >= build)))
	{
	  return 0;
	}
      else
	{
	  printf("\n*** An old version of OpenHBCI (%d.%d.%d build %d) was found.\n",
		 getmajor, getminor, getmicro, getbuild);
	  printf("*** You need OpenHBCI in version %d.%d.%d build %d or newer. The latest\n",
		 major, minor, micro, build);
	  printf("*** version of OpenHBCI is always available from http://www.openhbci.de.\n");
	  if (build > 0)
	    printf("*** Note: The non-zero build number '%d' indicates that the latest CVS \n"
		   "*** version of openhbci is required.\n", build);
	  printf("***\n"
	"*** If you have already installed a sufficiently new version, this error\n"
	"*** probably means that you need to specify the right path by\n"
	"*** --with-openhbci-prefix=PATH.  (Also, check that your LD_LIBRARY_PATH\n"
	"*** enviroment variable is correct, or edit /etc/ld.so.conf\n"
	"*** so that the correct libraries are found at run-time)\n");
	}
    }
  return 1;
}
],, no_openhbci=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
  CXXFLAGS="$ac_save_CXXFLAGS"
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
    ac_save_CXXFLAGS="$CXXFLAGS"
    ac_save_LIBS="$LIBS"
    CXXFLAGS="$CXXFLAGS $OPENHBCI_CXXFLAGS"
    LIBS="$LIBS $OPENHBCI_LIBS"
    AC_TRY_LINK([
#include <openhbci.h>
#include <stdio.h>
],      [ return ((OPENHBCI_VERSION_MAJOR) || (OPENHBCI_VERSION_MINOR)); ],
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
    CXXFLAGS="$ac_save_CXXFLAGS"
    LIBS="$ac_save_LIBS"
  fi
  OPENHBCI_CXXFLAGS=""
  OPENHBCI_LIBS=""
  ifelse([$3], , :, [$3])
  exit 1;
fi
AC_SUBST(OPENHBCI_CFLAGS)
AC_SUBST(OPENHBCI_LIBS)
AC_SUBST(OPENHBCI_CXXFLAGS)
AC_LANG_POP(C++)
rm -f conf.openhbcitest
])
