dnl GNOME_SUPPORT_CHECKS
dnl    Check for various support functions needed by the standard
dnl    Gnome libraries.  Sets LIBOBJS, might define some macros.
dnl    This should only be used when building the Gnome libs; 
dnl    Gnome clients should not need this macro.
AC_DEFUN([GNOME_SUPPORT_CHECKS],[
  # we need an `awk' to build `gnomesupport.h'
  AC_REQUIRE([AC_PROG_AWK])

  # this should go away soon
  need_gnome_support=yes

  save_LIBOBJS="$LIBOBJS"
  LIBOBJS=

  AC_CHECK_FUNCS(getopt_long,,LIBOBJS="$LIBOBJS getopt.o getopt1.o")

  # for `scandir'
  AC_HEADER_DIRENT

  # copied from `configure.in' of `libiberty'
  vars="program_invocation_short_name program_invocation_name sys_errlist"
  for v in $vars; do
    AC_MSG_CHECKING([for $v])
    AC_CACHE_VAL(gnome_cv_var_$v,
      [AC_TRY_LINK([int *p;], [extern int $v; p = &$v;],
		   [eval "gnome_cv_var_$v=yes"],
		   [eval "gnome_cv_var_$v=no"])])
    if eval "test \"`echo '$gnome_cv_var_'$v`\" = yes"; then
      AC_MSG_RESULT(yes)
      n=HAVE_`echo $v | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
      AC_DEFINE_UNQUOTED($n)
    else
      AC_MSG_RESULT(no)
    fi
  done

  AC_REPLACE_FUNCS(memmove mkstemp scandir strcasecmp strerror strndup strnlen)
  AC_REPLACE_FUNCS(strtok_r strtod strtol strtoul vasprintf vsnprintf)

  AC_CHECK_FUNCS(realpath,,LIBOBJS="$LIBOBJS canonicalize.o")

  # to include `error.c' error.c has some HAVE_* checks
  AC_CHECK_FUNCS(vprintf doprnt strerror_r)
  AM_FUNC_ERROR_AT_LINE

  # This is required if we declare setreuid () and setregid ().
  AC_TYPE_UID_T

  # see if we need to declare some functions.  Solaris is notorious for
  # putting functions into the `libc' but not listing them in the headers
  AC_CHECK_HEADERS(string.h strings.h stdlib.h unistd.h dirent.h)
  GCC_NEED_DECLARATIONS(gethostname setreuid setregid getpagesize)
  GCC_NEED_DECLARATION(scandir,[
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
])

  # Turn our LIBOBJS into libtool objects.  This is gross, but it
  # requires changes to autoconf before it goes away.
  LTLIBOBJS=`echo "$LIBOBJS" | sed 's/\.o/.lo/g'`
  AC_SUBST(need_gnome_support)
  AC_SUBST(LTLIBOBJS)

  LIBOBJS="$save_LIBOBJS"
  AM_CONDITIONAL(BUILD_GNOME_SUPPORT, test "$need_gnome_support" = yes)
])
