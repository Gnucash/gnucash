dnl
dnl Check for struct linger
dnl
AC_DEFUN(AC_STRUCT_LINGER, [
av_struct_linger=no
AC_MSG_CHECKING(struct linger is available)
AC_TRY_RUN([
#include <sys/types.h>
#include <sys/socket.h>

struct linger li;

main ()
{
    li.l_onoff = 1;
    li.l_linger = 120;
    exit (0);
}
],[
AC_DEFINE(HAVE_STRUCT_LINGER)
av_struct_linger=yes
],[
av_struct_linger=no
],[
av_struct_linger=no
])
AC_MSG_RESULT($av_struct_linger)
])
