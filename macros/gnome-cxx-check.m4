dnl GNOME_CHECK_CXX(not_found_string)
AC_DEFUN([GNOME_CHECK_CXX],
[
  # see if a C++ compiler exists and works
  AC_REQUIRE([AC_PROG_CXX])dnl
  if test "x$ac_cv_prog_cxx_works" = xno; then
    AC_MSG_WARN(ifelse([$1], , "No C++ compiler", [$1]))
  fi
  AM_CONDITIONAL(CXX_PRESENT, test "x$ac_cv_prog_cxx_works" != xno)
])
