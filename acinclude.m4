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
 
 dnl If guile uses the old-style smob, then define it.
 dnl To make this work, you must have the line 
 dnl #undef GWRAP_OLD_GUILE_SMOB 
 dnl in your acconfig.h or config.h.in, and
 dnl include that config.h.  If not, your code will not
 dnl work with guile 1.3
 AC_MSG_CHECKING(for whether guile uses old SMOB format)
 if ${GUILE} -c '(if (string=? (version) "1.3") (exit 0) (exit 1))' ;
 then
	AC_MSG_RESULT(yes)
        AC_DEFINE(GWRAP_OLD_GUILE_SMOB)
 else
	AC_MSG_RESULT(no)
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
   G_WRAP_CONFIG = ${gwrap_prefix}/bin/g-wrap-config
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
