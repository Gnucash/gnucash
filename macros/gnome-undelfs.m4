dnl GNOME_UNDELFS_CHECKS
dnl    Check for ext2fs undel support.
dnl    Set shell variable ext2fs_undel to "yes" if we have it,
dnl    "no" otherwise.  May define USE_EXT2FSLIB for cpp.
dnl    Will set EXT2FS_UNDEL_LIBS to required libraries.

AC_DEFUN([GNOME_UNDELFS_CHECKS], [
  AC_CHECK_HEADERS(ext2fs/ext2fs.h linux/ext2_fs.h)
  ext2fs_undel=no
  EXT2FS_UNDEL_LIBS=
  if test x$ac_cv_header_ext2fs_ext2fs_h = xyes
  then
    if test x$ac_cv_header_linux_ext2_fs_h = xyes
    then
      AC_DEFINE(USE_EXT2FSLIB)
      ext2fs_undel=yes
      EXT2FS_UNDEL_LIBS="-lext2fs -lcom_err"
    fi
  fi
])
