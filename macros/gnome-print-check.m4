# Configure paths for GNOME-PRINT
# Chris Lahey	99-2-5
# stolen from Manish Singh again
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_GNOME_PRINT([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GNOME-PRINT, and define GNOME_PRINT_CFLAGS and GNOME_PRINT_LIBS
dnl
AC_DEFUN([AM_PATH_GNOME_PRINT],
[
  min_version=ifelse([$1],,0.21,$1)

  gnome_print_ok=""

  AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
  if test "$GNOME_CONFIG" = "no" ; then
    AC_MSG_RESULT(gnome-config is missing, check your gnome installation)
  else
    AC_MSG_CHECKING(for GNOME-PRINT - version >= $min_version)
    if `$GNOME_CONFIG --libs print > /dev/null 2>&1`; then
      rqmajor=`echo "$min_version" | sed -e 's/cvs-//' | sed 's/\([[0-9]]*\)\.\([[0-9]]*\).*/\1/'`
      rqminor=`echo "$min_version" | sed -e 's/cvs-//' | sed 's/\([[0-9]]*\)\.\([[0-9]]*\).*/\2/'`
      major=`$GNOME_CONFIG --modversion print | sed -e 's/gnome-print-//' | sed -e 's/cvs-//' | sed 's/\([[0-9]]*\)\.\([[0-9]]*\).*/\1/'`
      minor=`$GNOME_CONFIG --modversion print | sed -e 's/gnome-print-//' | sed -e 's/cvs-//' | sed 's/\([[0-9]]*\)\.\([[0-9]]*\).*/\2/'`
      if test "$major" -ge "$rqmajor"; then
        if test "$major" -gt "$rqmajor"; then
          AC_MSG_RESULT("found $major.$minor")
          gnome_print_ok="yes"
        else
          if test "$minor" -ge "$rqminor"; then
            AC_MSG_RESULT("found $major.$minor")
            gnome_print_ok="yes"
          else
            AC_MSG_RESULT("you have $major.$minor")
          fi
        fi
      else
        AC_MSG_RESULT("you have $major.$minor")
      fi
    else
      AC_MSG_RESULT("did not find any version")
    fi
  fi

  if test "x$gnome_print_ok" != "x" ; then
    GNOME_PRINT_CFLAGS=`$GNOME_CONFIG --cflags print`
    GNOME_PRINT_LIBS=`$GNOME_CONFIG --libs print`
    ifelse([$2], , :, [$2])
  else
     GNOME_PRINT_CFLAGS=""
     GNOME_PRINT_LIBS=""
     ifelse([$3], , :, [$3])
  fi

  AC_SUBST(GNOME_PRINT_CFLAGS)
  AC_SUBST(GNOME_PRINT_LIBS)
])

AC_DEFUN([GNOME_PRINT_CHECK], [
	AM_PATH_GNOME_PRINT($1,,[AC_MSG_ERROR(GNOME-PRINT not found or wrong version)])
])
