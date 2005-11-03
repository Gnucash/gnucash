dnl @synopsis VL_LIB_READLINE
dnl
dnl Searches for a readline compatible library.  If found, defines
dnl `HAVE_LIBREADLINE'.  If the found library has the `add_history'
dnl function, sets also `HAVE_READLINE_HISTORY'.  Also checks for the
dnl locations of the necessary include files and sets `HAVE_READLINE_H'
dnl or `HAVE_READLINE_READLINE_H' and `HAVE_READLINE_HISTORY_H' or
dnl 'HAVE_HISTORY_H' if the corresponding include files exists.
dnl
dnl The libraries that may be readline compatible are `libedit',
dnl `libeditline' and `libreadline'.  Sometimes we need to link a termcap
dnl library for readline to work, this macro tests these cases too by
dnl trying to link with `libtermcap', `libcurses' or `libncurses' before
dnl giving up.
dnl
dnl Here is an example of how to use the information provided by this
dnl macro to perform the necessary includes or declarations in a C file:
dnl
dnl   #ifdef HAVE_LIBREADLINE
dnl   #  if defined(HAVE_READLINE_READLINE_H)
dnl   #    include <readline/readline.h>
dnl   #  elif defined(HAVE_READLINE_H)
dnl   #    include <readline.h>
dnl   #  else /* !defined(HAVE_READLINE_H) */
dnl   extern char *readline ();
dnl   #  endif /* !defined(HAVE_READLINE_H) */
dnl   char *cmdline = NULL;
dnl   #else /* !defined(HAVE_READLINE_READLINE_H) */
dnl     /* no readline */
dnl   #endif /* HAVE_LIBREADLINE */
dnl
dnl   #ifdef HAVE_READLINE_HISTORY
dnl   #  if defined(HAVE_READLINE_HISTORY_H)
dnl   #    include <readline/history.h>
dnl   #  elif defined(HAVE_HISTORY_H)
dnl   #    include <history.h>
dnl   #  else /* !defined(HAVE_HISTORY_H) */
dnl   extern void add_history ();
dnl   extern int write_history ();
dnl   extern int read_history ();
dnl   #  endif /* defined(HAVE_READLINE_HISTORY_H) */
dnl     /* no history */
dnl   #endif /* HAVE_READLINE_HISTORY */
dnl
dnl
dnl @version 1.1
dnl @author Ville Laurikari <vl@iki.fi>
dnl
dnl Modifications by T. Joseph Carter for pilot-link:
dnl  - Don't put -lreadline in LIBS
dnl  - Add --without-readline to disable use of it
dnl  - Set msg_readline and RL_LIBS appropriately
dnl
dnl  TODO:
dnl   - Add --with-readline-includes and -headers
dnl   - Make the above default to $with_readline/{include,lib} if set to
dnl     something other than "yes" or "no".
dnl
AC_DEFUN([VL_LIB_READLINE], [
  msg_readline="no"
  RL_LIBS=""
  AC_ARG_WITH(readline,
      [  --without-readline      Disable readline            [[default=no]]],,
      with_readline="yes")
  if test "x$with_readline" != "xno"; then

    AC_CACHE_CHECK([for a readline compatible library],
                   vl_cv_lib_readline, [
      ORIG_LIBS="$LIBS"
      for readline_lib in readline edit editline; do
        for termcap_lib in "" termcap curses ncurses; do
          if test -z "$termcap_lib"; then
            TRY_LIB="-l$readline_lib"
          else
            TRY_LIB="-l$readline_lib -l$termcap_lib"
          fi
          LIBS="$ORIG_LIBS $TRY_LIB"
          AC_TRY_LINK_FUNC(readline, vl_cv_lib_readline="$TRY_LIB")
          if test -n "$vl_cv_lib_readline"; then
            break
          fi
        done
        if test -n "$vl_cv_lib_readline"; then
          break
        fi
      done
      if test -z "$vl_cv_lib_readline"; then
        vl_cv_lib_readline="no"
      fi
      LIBS="$ORIG_LIBS"
    ])
  fi

  if test "$vl_cv_lib_readline" != "no"; then
    msg_readline="yes"
    RL_LIBS="$vl_cv_lib_readline"
    AC_DEFINE(HAVE_LIBREADLINE, 1,
              [Define if you have a readline compatible library])
    AC_CHECK_HEADERS(readline.h readline/readline.h)
    AC_CACHE_CHECK([whether readline supports history],
                   vl_cv_lib_readline_history, [
      vl_cv_lib_readline_history="no"
      AC_TRY_LINK_FUNC(add_history, vl_cv_lib_readline_history="yes")
    ])
    if test "$vl_cv_lib_readline_history" = "yes"; then
      AC_DEFINE(HAVE_READLINE_HISTORY, 1,
                [Define if your readline library has \`add_history'])
      AC_CHECK_HEADERS(history.h readline/history.h)
    fi
  fi
  AC_SUBST(RL_LIBS)
])dnl
