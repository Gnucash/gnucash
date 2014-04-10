dnl
dnl GNOME_PILOT_HOOK(script if found, fail)
dnl if fail = "failure", abort if gnome-pilot not found
dnl

dnl
dnl 

GNOME_PILOT_CFLAGS=
GNOME_PILOT_LIBS=
PISOCK_CFLAGS=
PISOCK_LIBS=

AC_SUBST(GNOME_PILOT_CFLAGS)
AC_SUBST(GNOME_PILOT_LIBS)
AC_SUBST(PISOCK_CFLAGS)
AC_SUBST(PISOCK_LIBS)

AC_DEFUN([PILOT_LINK_HOOK],[
	AC_ARG_WITH(pisock,
	[  --with-pisock            Specify prefix for pisock files],[
	if test x$withval = xyes; then
	    dnl Note that an empty true branch is not valid sh syntax.
	    ifelse([$1], [], :, [$1])
	else
	    PISOCK_CFLAGS="-I$withval/include"
	    incdir="$withval/include"
	    PISOCK_LIBS="-L$withval/lib -lpisock"
	    AC_MSG_CHECKING("for existance of $withval/lib/libpisock.so")
	    if test -r $withval/lib/libpisock.so; then
		AC_MSG_RESULT("yes")
	    else
		AC_MSG_ERROR("Unable to find libpisock. Try ftp://ryeham.ee.ryerson.ca/pub/PalmOS/.")
	    fi
	fi
	])

	if test x$PISOCK_CFLAGS = x; then
	    AC_CHECK_HEADER(pi-version.h, [incdir="/usr/include"], [
	    AC_CHECK_HEADER(libpisock/pi-version.h, [PISOCK_CFLAGS="-I/usr/include/libpisock"
	                                             piversion_include="libpisock/pi-version.h"
						     incdir="/usr/inlude/libpisock"
                                                    ], [
	    AC_CHECK_HEADER($prefix/include/pi-version.h, [PISOCK_CFLAGS="-I$prefix/include/libpisock"
	                                                   piversion_include="$prefix/include/pi-version.h"
						           if test x$PISOCK_LIBDIR = x; then
							      echo Assuming libpisock in $prefix/lib
							      incdir="$prefix/include"
							      PISOCK_LIBS="-L$prefix/lib -lpisock"
                                                           fi							  ],
	    AC_MSG_ERROR("Unable to find pi-version.h")) 
	    ])
	    ])
	fi
		
	if test "x$PISOCK_LIBS" = "x"; then
		AC_CHECK_LIB(pisock, pi_accept, [ PISOCK_LIBS=-lpisock ], 
			[ AC_MSG_ERROR("Unable to find libpisock. Try ftp://ryeham.ee.ryerson.ca/pub/PalmOS/.") ])
	fi
	
	AC_ARG_ENABLE(pilotlinktest,
		[  --enable-pilotlinktest   Test for correct version of pilot-link],
		[testplversion=$enableval],
		[ testplversion=yes ]
	)

	if test x$piversion_include = x; then
		piversion_include="pi-version.h"
		pi_version=`cat $incdir/pi-version.h|grep PILOT_LINK_VERSION|sed 's/#define PILOT_LINK_VERSION \([[0-9]]*\)/\1/'`
		pi_major=`cat $incdir/pi-version.h|grep PILOT_LINK_MAJOR|sed 's/#define PILOT_LINK_MAJOR \([[0-9]]*\)/\1/'`
		pi_minor=`cat $incdir/pi-version.h|grep PILOT_LINK_MINOR|sed 's/#define PILOT_LINK_MINOR \([[0-9]]*\)/\1/'`
		pi_patch=`cat $incdir/pi-version.h|grep PILOT_LINK_PATCH|sed 's/#define PILOT_LINK_PATCH \"\(.*\)\"/\1/'`
	fi

	PILOT_LINK_VERSION="$pi_version.$pi_major.$pi_minor$pi_patch"

	if test x$testplversion = xyes; then
		AC_MSG_CHECKING(for pilot-link version >= $1)
		pl_ve=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
		pl_ma=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
		pl_mi=`echo $1|sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
		CFLAGS_save="$CFLAGS"
		CFLAGS="$CFLAGS $PISOCK_CFLAGS"
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
			[AC_MSG_RESULT(yes (found $PILOT_LINK_VERSION))],
			[AC_MSG_ERROR("pilot-link >= $1 required")],
			[AC_MSG_WARN("No action taken for crosscompile")]
		)
		CFLAGS="$CFLAGS_save"
	fi

	PISOCK_CFLAGS="$PISOCK_CFLAGS -DGP_PILOT_LINK_VERSION=\\\"$PILOT_LINK_VERSION\\\""

	unset piversion_include
	unset pi_verion
	unset pi_major
	unset pi_minor
	unset pi_patch
	unset incdir
	unset pl_mi
	unset pl_ma
	unset pl_ve
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
		GNOME_PILOT_HOOK(0.9.5,[],nofailure)
	else
		GNOME_PILOT_HOOK($1,[],nofailure)
	fi
])

