dnl
dnl GNOME_FILEUTILS_CHECKS
dnl
dnl checks that are needed for the diskusage applet.
dnl

AC_DEFUN([GNOME_FILEUTILS_CHECKS],
[	
AC_CHECK_HEADERS(fcntl.h sys/param.h sys/statfs.h sys/fstyp.h \
mnttab.h mntent.h sys/statvfs.h sys/vfs.h sys/mount.h \
sys/filsys.h sys/fs_types.h sys/fs/s5param.h)

AC_CHECK_FUNCS(bcopy endgrent endpwent fchdir ftime ftruncate \
getcwd getmntinfo gettimeofday isascii lchown \
listmntent memcpy mkfifo strchr strerror strrchr vprintf)

dnl Set some defaults when cross-compiling

if test x$cross_compiling = xyes ; then
	case "$host_os" in
	linux*)
	  fu_cv_sys_mounted_getmntent1=yes
	  fu_cv_sys_stat_statfs2_bsize=yes
	  ;;
	sunos*)
	  fu_cv_sys_stat_statfs4=yes
	  ;;
	freebsd*)
	  fu_cv_sys_stat_statfs2_bsize=yes
	  ;;
	osf*)
	  fu_cv_sys_stat_statfs3_osf1=yes
	  ;;
	esac
fi

# Determine how to get the list of mounted filesystems.
list_mounted_fs=

# If the getmntent function is available but not in the standard library,
# make sure LIBS contains -lsun (on Irix4) or -lseq (on PTX).
AC_FUNC_GETMNTENT

# This test must precede the ones for getmntent because Unicos-9 is
# reported to have the getmntent function, but its support is incompatible
# with other getmntent implementations.

# NOTE: Normally, I wouldn't use a check for system type as I've done for
# `CRAY' below since that goes against the whole autoconf philosophy.  But
# I think there is too great a chance that some non-Cray system has a
# function named listmntent to risk the false positive.

if test -z "$list_mounted_fs"; then
# Cray UNICOS 9
AC_MSG_CHECKING([for listmntent of Cray/Unicos-9])
AC_CACHE_VAL(fu_cv_sys_mounted_cray_listmntent,
[fu_cv_sys_mounted_cray_listmntent=no
AC_EGREP_CPP(yes,
[#ifdef _CRAY
yes
#endif
], [test $ac_cv_func_listmntent = yes \
&& fu_cv_sys_mounted_cray_listmntent=yes]
)
]
)
AC_MSG_RESULT($fu_cv_sys_mounted_cray_listmntent)
if test $fu_cv_sys_mounted_cray_listmntent = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_LISTMNTENT)
fi
fi

if test $ac_cv_func_getmntent = yes; then

# This system has the getmntent function.
# Determine whether it's the one-argument variant or the two-argument one.

if test -z "$list_mounted_fs"; then
# 4.3BSD, SunOS, HP-UX, Dynix, Irix
AC_MSG_CHECKING([for one-argument getmntent function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmntent1,
[test $ac_cv_header_mntent_h = yes \
&& fu_cv_sys_mounted_getmntent1=yes \
|| fu_cv_sys_mounted_getmntent1=no])
AC_MSG_RESULT($fu_cv_sys_mounted_getmntent1)
if test $fu_cv_sys_mounted_getmntent1 = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNTENT1)
fi
fi

if test -z "$list_mounted_fs"; then
# SVR4
AC_MSG_CHECKING([for two-argument getmntent function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmntent2,
[AC_EGREP_HEADER(getmntent, sys/mnttab.h,
fu_cv_sys_mounted_getmntent2=yes,
fu_cv_sys_mounted_getmntent2=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_getmntent2)
if test $fu_cv_sys_mounted_getmntent2 = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNTENT2)
fi
fi

if test -z "$list_mounted_fs"; then
AC_MSG_ERROR([could not determine how to read list of mounted filesystems])
fi

fi

if test -z "$list_mounted_fs"; then
# DEC Alpha running OSF/1.
AC_MSG_CHECKING([for getfsstat function])
AC_CACHE_VAL(fu_cv_sys_mounted_getsstat,
[AC_TRY_LINK([
#include <sys/types.h>
#include <sys/mount.h>
#include <sys/fs_types.h>],
[struct statfs *stats;
int numsys = getfsstat ((struct statfs *)0, 0L, MNT_WAIT); ],
fu_cv_sys_mounted_getsstat=yes,
fu_cv_sys_mounted_getsstat=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_getsstat)
if test $fu_cv_sys_mounted_getsstat = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETFSSTAT)
fi
fi

if test -z "$list_mounted_fs"; then
# AIX.
AC_MSG_CHECKING([for mntctl function and struct vmount])
AC_CACHE_VAL(fu_cv_sys_mounted_vmount,
[AC_TRY_CPP([#include <fshelp.h>],
fu_cv_sys_mounted_vmount=yes,
fu_cv_sys_mounted_vmount=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_vmount)
if test $fu_cv_sys_mounted_vmount = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_VMOUNT)
fi
fi

if test -z "$list_mounted_fs"; then
# SVR3
AC_MSG_CHECKING([for FIXME existence of three headers])
AC_CACHE_VAL(fu_cv_sys_mounted_fread_fstyp,
[AC_TRY_CPP([
#include <sys/statfs.h>
#include <sys/fstyp.h>
#include <mnttab.h>],
fu_cv_sys_mounted_fread_fstyp=yes,
fu_cv_sys_mounted_fread_fstyp=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_fread_fstyp)
if test $fu_cv_sys_mounted_fread_fstyp = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_FREAD_FSTYP)
fi
fi

if test -z "$list_mounted_fs"; then
# 4.4BSD and DEC OSF/1.
AC_MSG_CHECKING([for getmntinfo function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmntinfo,
[
ok=
if test $ac_cv_func_getmntinfo = yes; then
AC_EGREP_HEADER(f_type;, sys/mount.h,
ok=yes)
fi
test -n "$ok" \
&& fu_cv_sys_mounted_getmntinfo=yes \
|| fu_cv_sys_mounted_getmntinfo=no
])
AC_MSG_RESULT($fu_cv_sys_mounted_getmntinfo)
if test $fu_cv_sys_mounted_getmntinfo = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNTINFO)
fi
fi

# FIXME: add a test for netbsd-1.1 here

if test -z "$list_mounted_fs"; then
# Ultrix
AC_MSG_CHECKING([for getmnt function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmnt,
[AC_TRY_CPP([
#include <sys/fs_types.h>
#include <sys/mount.h>],
fu_cv_sys_mounted_getmnt=yes,
fu_cv_sys_mounted_getmnt=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_getmnt)
if test $fu_cv_sys_mounted_getmnt = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNT)
fi
fi

if test -z "$list_mounted_fs"; then
# SVR2
AC_MSG_CHECKING([whether it is possible to resort to fread on /etc/mnttab])
AC_CACHE_VAL(fu_cv_sys_mounted_fread,
[AC_TRY_CPP([#include <mnttab.h>],
fu_cv_sys_mounted_fread=yes,
fu_cv_sys_mounted_fread=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_fread)
if test $fu_cv_sys_mounted_fread = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_FREAD)
fi
fi

if test -z "$list_mounted_fs"; then
AC_MSG_ERROR([could not determine how to read list of mounted filesystems])
# FIXME -- no need to abort building the whole package
# Can't build mountlist.c or anything that needs its functions
fi

AC_CHECKING(how to get filesystem space usage)
space=no

# Perform only the link test since it seems there are no variants of the
# statvfs function.  This check is more than just AC_CHECK_FUNCS(statvfs)
# because that got a false positive on SCO OSR5.  Adding the declaration
# of a `struct statvfs' causes this test to fail (as it should) on such
# systems.  That system is reported to work fine with STAT_STATFS4 which
# is what it gets when this test fails.
if test $space = no; then
# SVR4
AC_CACHE_CHECK([statvfs function (SVR4)], fu_cv_sys_stat_statvfs,
[AC_TRY_LINK([#include <sys/types.h>
#include <sys/statvfs.h>],
[struct statvfs fsd; statvfs (0, &fsd);],
fu_cv_sys_stat_statvfs=yes,
fu_cv_sys_stat_statvfs=no)])
if test $fu_cv_sys_stat_statvfs = yes; then
space=yes
AC_DEFINE(STAT_STATVFS)
fi
fi

if test $space = no; then
# DEC Alpha running OSF/1
AC_MSG_CHECKING([for 3-argument statfs function (DEC OSF/1)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs3_osf1,
[AC_TRY_RUN([
#include <sys/param.h>
#include <sys/types.h>
#include <sys/mount.h>
main ()
{
struct statfs fsd;
fsd.f_fsize = 0;
exit (statfs (".", &fsd, sizeof (struct statfs)));
}],
fu_cv_sys_stat_statfs3_osf1=yes,
fu_cv_sys_stat_statfs3_osf1=no,
fu_cv_sys_stat_statfs3_osf1=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs3_osf1)
if test $fu_cv_sys_stat_statfs3_osf1 = yes; then
space=yes
AC_DEFINE(STAT_STATFS3_OSF1)
fi
fi

if test $space = no; then
# AIX
AC_MSG_CHECKING([for two-argument statfs with statfs.bsize dnl
member (AIX, 4.3BSD)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs2_bsize,
[AC_TRY_RUN([
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
main ()
{
struct statfs fsd;
fsd.f_bsize = 0;
exit (statfs (".", &fsd));
}],
fu_cv_sys_stat_statfs2_bsize=yes,
fu_cv_sys_stat_statfs2_bsize=no,
fu_cv_sys_stat_statfs2_bsize=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs2_bsize)
if test $fu_cv_sys_stat_statfs2_bsize = yes; then
space=yes
AC_DEFINE(STAT_STATFS2_BSIZE)
fi
fi

if test $space = no; then
# SVR3
AC_MSG_CHECKING([for four-argument statfs (AIX-3.2.5, SVR3)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs4,
[AC_TRY_RUN([#include <sys/types.h>
#include <sys/statfs.h>
main ()
{
struct statfs fsd;
exit (statfs (".", &fsd, sizeof fsd, 0));
}],
fu_cv_sys_stat_statfs4=yes,
fu_cv_sys_stat_statfs4=no,
fu_cv_sys_stat_statfs4=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs4)
if test $fu_cv_sys_stat_statfs4 = yes; then
space=yes
AC_DEFINE(STAT_STATFS4)
fi
fi

if test $space = no; then
# 4.4BSD and NetBSD
AC_MSG_CHECKING([for two-argument statfs with statfs.fsize dnl
member (4.4BSD and NetBSD)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs2_fsize,
[AC_TRY_RUN([#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
main ()
{
struct statfs fsd;
fsd.f_fsize = 0;
exit (statfs (".", &fsd));
}],
fu_cv_sys_stat_statfs2_fsize=yes,
fu_cv_sys_stat_statfs2_fsize=no,
fu_cv_sys_stat_statfs2_fsize=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs2_fsize)
if test $fu_cv_sys_stat_statfs2_fsize = yes; then
space=yes
AC_DEFINE(STAT_STATFS2_FSIZE)
fi
fi

if test $space = no; then
# Ultrix
AC_MSG_CHECKING([for two-argument statfs with struct fs_data (Ultrix)])
AC_CACHE_VAL(fu_cv_sys_stat_fs_data,
[AC_TRY_RUN([#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_FS_TYPES_H
#include <sys/fs_types.h>
#endif
main ()
{
struct fs_data fsd;
/* Ultrix's statfs returns 1 for success,
0 for not mounted, -1 for failure.  */
exit (statfs (".", &fsd) != 1);
}],
fu_cv_sys_stat_fs_data=yes,
fu_cv_sys_stat_fs_data=no,
fu_cv_sys_stat_fs_data=no)])
AC_MSG_RESULT($fu_cv_sys_stat_fs_data)
if test $fu_cv_sys_stat_fs_data = yes; then
space=yes
AC_DEFINE(STAT_STATFS2_FS_DATA)
fi
fi

if test $space = no; then
# SVR2
AC_TRY_CPP([#include <sys/filsys.h>],
AC_DEFINE(STAT_READ_FILSYS) space=yes)
fi

if test -n "$list_mounted_fs" && test $space != no; then
DF_PROG="df"
# LIBOBJS="$LIBOBJS fsusage.o"
# LIBOBJS="$LIBOBJS mountlist.o"
fi

# Check for SunOS statfs brokenness wrt partitions 2GB and larger.
# If <sys/vfs.h> exists and struct statfs has a member named f_spare,
# enable the work-around code in fsusage.c.
AC_MSG_CHECKING([for statfs that truncates block counts])
AC_CACHE_VAL(fu_cv_sys_truncating_statfs,
[AC_TRY_COMPILE([
#if !defined(sun) && !defined(__sun)
choke -- this is a workaround for a Sun-specific problem
#endif
#include <sys/types.h>
#include <sys/vfs.h>],
[struct statfs t; long c = *(t.f_spare);],
fu_cv_sys_truncating_statfs=yes,
fu_cv_sys_truncating_statfs=no,
)])
if test $fu_cv_sys_truncating_statfs = yes; then
AC_DEFINE(STATFS_TRUNCATES_BLOCK_COUNTS)
fi
AC_MSG_RESULT($fu_cv_sys_truncating_statfs)

AC_CHECKING(for AFS)
test -d /afs && AC_DEFINE(AFS)
])
