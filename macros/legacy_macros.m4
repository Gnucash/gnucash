## -*-m4-*-

dnl AM_GUILE_VERSION_CHECK ([MINIMUM-VERSION, MAXIMUM-VERSION, [ACTION-IF-FOUND.
dnl	           [ACTION-IF-NOT-FOUND]]])

dnl tests for minimum and maximum versions of guile.

AC_DEFUN([AM_GUILE_VERSION_CHECK],
[
dnl
dnl

if test x${GUILE} = x ; then
   AC_PATH_PROG(GUILE, guile, no)
fi

version_ok=
min_guile_version=ifelse([$1], , 1.3,$1)
max_guile_version=ifelse([$2], , 99.99.99,$2)

AC_MSG_CHECKING(for guile - ${min_guile_version} <= version < ${max_guile_version})

if test x${BUILD_GUILE} != x -a ${BUILD_GUILE} != no ; then
  guile_version=`${name_build_guile} --version 2>&1`
  guile_version="$guile_version.0"
  guile_major_version=`echo $guile_version | \
	sed 's/.*Guile version \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\).*/\1/'`
  guile_minor_version=`echo $guile_version | \
	sed 's/.*Guile version \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\).*/\2/'`
  guile_micro_version=`echo $guile_version | \
	sed 's/.*Guile version \([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\).*/\3/'`
  guile_vers_string="$guile_major_version.$guile_minor_version.$guile_micro_version"

  major_required=`echo ${min_guile_version} |\
        sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
  minor_required=`echo ${min_guile_version} |\
	sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
  micro_required=`echo ${min_guile_version} |\
	sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

  major_prohibited=`echo ${max_guile_version} |\
        sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
  minor_prohibited=`echo ${max_guile_version} |\
	sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
  micro_prohibited=`echo ${max_guile_version} |\
	sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`


  if ${GUILE} -c "(cond ((> ${guile_major_version} ${major_required}) (exit 0))\
	           ((< ${guile_major_version} ${major_required}) (exit 1))\
                   ((> ${guile_minor_version} ${minor_required}) (exit 0))\
		   ((< ${guile_minor_version} ${minor_required}) (exit 1))\
	           ((< ${guile_micro_version} ${micro_required}) (exit 1))\
		   (else (exit 0)))" ; then
    if ${GUILE} -c "(cond ((> ${guile_major_version} ${major_prohibited}) (exit 1))\
	           ((< ${guile_major_version} ${major_prohibited}) (exit 0))\
                   ((> ${guile_minor_version} ${minor_prohibited}) (exit 1))\
		   ((< ${guile_minor_version} ${minor_prohibited}) (exit 0))\
	           ((< ${guile_micro_version} ${micro_prohibited}) (exit 0))\
		   (else (exit 1)))" ; then
  	version_ok=yes
    fi
  fi
fi

if test -n "$version_ok"; then
	AC_MSG_RESULT(yes: $guile_vers_string)
	ifelse([$3], , true, [$3])

else
	AC_MSG_RESULT(no: $guile_vers_string)
	ifelse([$4], , true , [AC_MSG_WARN(guile version check failed)
	$4])
fi])

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
  long long int e;

  d = 0;
  e =  100000;
  e *= 100000;
  if ((sscanf ("10000000000", "%lld", &d) != 1) || (d != e))
    exit (1);

  exit (0);
}
],
        am_cv_scanf_lld=yes,
        am_cv_scanf_lld=no,[[
	# When cross-compiling, simply insert known values here
	case $host in
	  *-*-mingw*)
	    # For mingw we know the result
	    am_cv_scanf_lld=no
	    ;;
	  *)
	    AC_MSG_ERROR([scanf support unknown.])
	    ;;
	esac
]]))
  if test $am_cv_scanf_lld = yes; then
    AC_DEFINE(HAVE_SCANF_LLD, 1,
      [Define if scanf supports %lld conversions.])
  fi
])

AC_DEFUN([SCANF_QD_CHECK],
[
  AC_CACHE_CHECK([if scanf supports %qd conversions],
                 am_cv_scanf_qd,
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>

int main ()
{
  long long int d;
  long long int e;

  d = 0;
  e =  100000;
  e *= 100000;
  if ((sscanf ("10000000000", "%qd", &d) != 1) || (d != e))
    exit (1);

  exit (0);
}
],
        am_cv_scanf_qd=yes,
        am_cv_scanf_qd=no,[[
	# When cross-compiling, simply insert known values here
	case $host in
	  *-*-mingw*)
	    # For mingw we know the result
	    am_cv_scanf_qd=no
	    ;;
	  *)
	    AC_MSG_ERROR([scanf support unknown.])
	    ;;
	esac
]]))
  if test $am_cv_scanf_qd = yes; then
    AC_DEFINE(HAVE_SCANF_QD, 1,
      [Define if scanf supports %qd conversions.])
  fi
])

AC_DEFUN([SCANF_I64D_CHECK],
[
  AC_CACHE_CHECK([if scanf supports %I64d conversions],
                 am_cv_scanf_i64d,
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>

int main ()
{
  long long int d;
  long long int e;

  d = 0;
  e =  100000;
  e *= 100000;
  if ((sscanf ("10000000000", "%I64d", &d) != 1) || (d != e))
    exit (1);

  exit (0);
}
],
        am_cv_scanf_i64d=yes,
        am_cv_scanf_i64d=no,[[
	# When cross-compiling, simply insert known values here
	case $host in
	  *-*-mingw*)
	    # For mingw we know the result
	    am_cv_scanf_i64d=yes
	    ;;
	  *)
	    AC_MSG_ERROR([scanf support unknown.])
	    ;;
	esac
]]))
  if test $am_cv_scanf_i64d = yes; then
    AC_DEFINE(HAVE_SCANF_I64D, 1,
      [Define if scanf supports %I64d conversions.])
  fi
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
