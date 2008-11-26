dnl as-scrub-include.m4 0.0.1
dnl autostars m4 macro for scrubbing CFLAGS of system include dirs
dnl because gcc 3.x complains about including system including dirs
dnl
dnl thomas@apestaart.org
dnl
dnl This macro uses output of cpp -v and expects it to contain text that 
dnl looks a little bit like this:
dnl #include <...> search starts here:
dnl  /usr/local/include
dnl  /usr/lib/gcc-lib/i386-redhat-linux/3.2/include
dnl  /usr/include
dnl End of search list.

dnl AS_SCRUB_INCLUDE(VAR)
dnl example
dnl AS_SCRUB_INCLUDE(CFLAGS)
dnl will remove all system include dirs from the given CFLAGS

AC_DEFUN([AS_SCRUB_INCLUDE],
[
  GIVEN_CFLAGS=$[$1]
  INCLUDE_DIRS=`echo | cpp -v 2>&1`

  dnl remove everything from this output between the "starts here" and "End of"
  dnl line
  INCLUDE_DIRS=`echo $INCLUDE_DIRS | sed -e 's/.*<...> search starts here://' | sed -e 's/End of search list.*//'`
  for dir in $INCLUDE_DIRS; do
    GIVEN_CFLAGS=`echo $GIVEN_CFLAGS | sed -e "s;-I$dir ;;" | sed -e "s;-I$dir$;;"`
  done
  [$1]=$GIVEN_CFLAGS
])
