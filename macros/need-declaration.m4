dnl See whether we need a declaration for a function.
dnl GCC_NEED_DECLARATION(FUNCTION [, EXTRA-HEADER-FILES])
AC_DEFUN([GCC_NEED_DECLARATION],
[AC_MSG_CHECKING([whether $1 must be declared])
AC_CACHE_VAL(gcc_cv_decl_needed_$1,
[AC_TRY_COMPILE([
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
$2],
[char *(*pfn) = (char *(*)) $1],
eval "gcc_cv_decl_needed_$1=no", eval "gcc_cv_decl_needed_$1=yes")])
if eval "test \"`echo '$gcc_cv_decl_needed_'$1`\" = yes"; then
  AC_MSG_RESULT(yes)
  gcc_need_declarations="$gcc_need_declarations $1"
  gcc_tr_decl=NEED_DECLARATION_`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
  AC_DEFINE_UNQUOTED($gcc_tr_decl)
else
  AC_MSG_RESULT(no)
fi
])dnl

dnl Check multiple functions to see whether each needs a declaration.
dnl GCC_NEED_DECLARATIONS(FUNCTION... [, EXTRA-HEADER-FILES])
AC_DEFUN([GCC_NEED_DECLARATIONS],
[for ac_func in $1
do
GCC_NEED_DECLARATION($ac_func, $2)
done
]
)
