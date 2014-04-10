dnl
dnl GNOME_PILOT_HOOK(script if found, fail)
dnl if fail = "failure", abort if gnome-pilot not found
dnl

GNOME_PILOT_CFLAGS=
GNOME_PILOT_LIBS=
PISOCK_INCLUDEDIR=
PISOCK_LIBS=
PILOT_BINS=
PILOT_LIBS=

AC_SUBST(GNOME_PILOT_CFLAGS)
AC_SUBST(GNOME_PILOT_LIBS)
AC_SUBST(PISOCK_INCLUDEDIR)
AC_SUBST(PISOCK_LIBS)
AC_SUBST(PILOT_BINS)
AC_SUBST(PILOT_LIBS)

AC_DEFUN([PILOT_LINK_HOOK],[
	AC_ARG_WITH(pisock,
	[  --with-pisock            Specify prefix for pisock files],[
	if test x$withval = xyes; then
	    dnl Note that an empty true branch is not valid sh syntax.
	    ifelse([$1], [], :, [$1])
	else
	    PISOCK_INCLUDEDIR="-I$withval/include"
	    PISOCK_LIBDIR="-L$withval/lib"
	    PISOCK_LIBS="-lpisock"
	    AC_MSG_CHECKING("for existance of $withval/lib/libpisock.so")
	    if test -r $withval/lib/libpisock.so; then
		AC_MSG_RESULT("yes")
	    else
		AC_MSG_ERROR("Unable to find libpisock. Try ftp://ryeham.ee.ryerson.ca/pub/PalmOS/.")
	    fi
	    AC_SUBST(PISOCK_INCLUDEDIR)
	    AC_SUBST(PISOCK_LIBDIR)
	fi
	])

	if test x$PISOCK_INCLUDEDIR = x; then
	    AC_CHECK_HEADER(pi-version.h, [], [
	    AC_CHECK_HEADER(libpisock/pi-version.h, [PISOCK_INCLUDEDIR="-I/usr/include/libpisock"
	                                             piversion_include="libpisock/pi-version.h"],
	    AC_MSG_ERROR("Unable to find pi-version.h")) ])
	fi
	
	AC_SUBST(PISOCK_INCLUDEDIR)
	
	if test x$PISOCK_LIBDIR = x; then
		AC_CHECK_LIB(pisock, pi_accept, [ PISOCK_LIBS=-lpisock ], 
			[ AC_MSG_ERROR("Unable to find libpisock. Try ftp://ryeham.ee.ryerson.ca/pub/PalmOS/.") ])
	fi
	
	AC_ARG_ENABLE(pilotlinktest,
		[  --enable-pilotlinktest       Test for correct version of pilot-link],
		[testplversion=$enableval],
		[ testplversion=yes ]
	)

	if test x$piversion_include = x; then
		piversion_include="pi-version.h"
	fi

	if test x$testplversion = xyes; then
		AC_MSG_CHECKING(for pilot-link version >= $1)
		pl_ve=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
		pl_ma=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
		pl_mi=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
		CFLAGS_save="$CFLAGS"
		CFLAGS="$CFLAGS $PISOCK_INCLUDEDIR"
		AC_TRY_RUN(
			[
			#include <$piversion_include>
			int main(int argc,char *argv[]) {
				if (PILOT_LINK_VERSION == $pl_ve) {
					if (PILOT_LINK_MAJOR == $pl_ma) {
						if (PILOT_LINK_MINOR >= $pl_mi) {
							exit(0);
				       	  	}
					} else if (PILOT_LINK_MAJOR > $pl_ma) {
						exit(0);
					}
				} else if (PILOT_LINK_VERSION > $pl_ve) {
					exit(0);
				}
				exit(1);
			}
			],
			[AC_MSG_RESULT(yes)],
			[AC_MSG_ERROR("pilot-link >= $1 required")],
			[AC_MSG_WARN("No action taken for crosscompile")]
		)
		CFLAGS="$CFLAGS_save"
	fi
])

AC_DEFUN([PILOT_LINK_CHECK],[
	PILOT_LINK_HOOK($1,[],nofailure)
])

AC_DEFUN([GNOME_PILOT_HOOK],[
	AC_PATH_PROG(GNOME_CONFIG,gnome-config,no)
	AC_CACHE_CHECK([for gnome-pilot environment],gnome_cv_pilot_found,[
		if test "x$GNOME_CONFIG" = "xno"; then
			gnome_cv_pilot_found=no
		else
			# gnome-config doesn't return a useful error status,
			# so we check if it outputs anything to stderr
			if test "x`$GNOME_CONFIG gpilot 2>&1 > /dev/null`" = "x"; then
				gnome_cv_pilot_found=yes
			else
				gnome_cv_pilot_found=no
			fi
		fi
	])
	AM_CONDITIONAL(HAVE_GNOME_PILOT,test x$gnome_cv_pilot_found = xyes)
	if test x$gnome_cv_pilot_found = xyes; then
		PILOT_LINK_CHECK($1)
		GNOME_PILOT_CFLAGS=`gnome-config --cflags gpilot`
		GNOME_PILOT_LIBS=`gnome-config --libs gpilot`
		$2
	else
		if test x$3 = xfailure; then
			AC_MSG_ERROR(gnome-pilot development package not installed or installation problem)
		fi
	fi
])

AC_DEFUN([GNOME_PILOT_CHECK],[
	if test x$1 = x; then
		GNOME_PILOT_HOOK(0.9.3,[],nofailure)
	else
		GNOME_PILOT_HOOK($1,[],nofailure)
	fi
])

