dnl AC_CHECK_X_LIB(LIBRARY, FUNCTION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND
dnl              [, OTHER-X-LIBRARIES [, OTHER-LIBRARIES]]]])
dnl This macro is just a variation on AC_CHECK_LIB, but for X-related
dnl libraries (X is just a tad delicate). OTHER-X-LIBRARIES is a list
dnl of other X-related libraries that might be required to link with
dnl LIBRARY (e.g., -lXpm). OTHER-LIBRARIES is a list of other (non-X)
dnl libraries that might be required (e.g., -lm).
AC_DEFUN(AC_CHECK_X_LIB,
[AC_REQUIRE([AC_PATH_XTRA])dnl
AC_MSG_CHECKING([for $2 in -l$1])
dnl Use a cache variable name containing both the library and function name,
dnl because the test really is for library $1 defining function $2, not
dnl just for library $1.  Separate tests with the same $1 and different $2s
dnl may have different results.
ac_lib_var=`echo $1['_']$2 | sed 'y%./+-%__p_%'`
AC_CACHE_VAL(ac_cv_lib_$ac_lib_var,
[ac_save_LIBS="$LIBS"
LIBS="$X_LIBS -l$1 $5 $X_PRE_LIBS -lX11 $X_EXTRA_LIBS $6 $LIBS"
AC_TRY_LINK(dnl
ifelse([$2], [main], , dnl Avoid conflicting decl of main.
[/* Override any gcc2 internal prototype to avoid an error.  */
]ifelse(AC_LANG, CPLUSPLUS, [#ifdef __cplusplus
extern "C"
#endif
])dnl
[/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
char $2();
]),
            [$2()],
            eval "ac_cv_lib_$ac_lib_var=yes",
            eval "ac_cv_lib_$ac_lib_var=no")
LIBS="$ac_save_LIBS"
])dnl
if eval "test \"`echo '$ac_cv_lib_'$ac_lib_var`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$3], ,
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | sed -e 's/[^a-zA-Z0-9_]/_/g' \
    -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`
changequote([, ])dnl
  AC_DEFINE_UNQUOTED($ac_tr_lib)
  X_PRE_LIBS="-l$1 $X_PRE_LIBS"
], [$3])
else
  AC_MSG_RESULT(no)
ifelse([$4], , , [$4
])dnl
fi
])
