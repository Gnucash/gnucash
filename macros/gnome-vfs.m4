dnl GNOME_VFS_CHECKS
dnl   Check for various functions needed by libvfs.
dnl   This has various effects:
dnl     Sets GNOME_VFS_LIBS to libraries required
dnl     Sets termnet  to true or false depending on whether it is required.
dnl        If yes, defines USE_TERMNET.
dnl     Sets vfs_flags to "pretty" list of vfs implementations we include.
dnl     Sets shell variable use_vfs to yes (default, --with-vfs) or
dnl        "no" (--without-vfs).
dnl     Calls AC_SUBST(mcserv), which is either empty or "mcserv".

dnl Private define
AC_DEFUN([GNOME_WITH_VFS],[
  dnl FIXME: network checks should probably be in their own macro.
  AC_CHECK_LIB(nsl, t_accept)
  AC_CHECK_LIB(socket, socket)

  have_socket=no
  AC_CHECK_FUNCS(socket, have_socket=yes)
  if test $have_socket = no; then
    # socket is not in the default libraries.  See if it's in some other.
    for lib in bsd socket inet; do
      AC_CHECK_LIB($lib, socket, [
	  LIBS="$LIBS -l$lib"
	  have_socket=yes
	  AC_DEFINE(HAVE_SOCKET)
	  break])
    done
  fi

  have_gethostbyname=no
  AC_CHECK_FUNC(gethostbyname, have_gethostbyname=yes)
  if test $have_gethostbyname = no; then
    # gethostbyname is not in the default libraries.  See if it's in some other.
    for lib in bsd socket inet; do
      AC_CHECK_LIB($lib, gethostbyname, [LIBS="$LIBS -l$lib"; have_gethostbyname=yes; break])
    done
  fi

  vfs_flags="tarfs"
  use_net_code=false
  if test $have_socket = yes; then
      AC_STRUCT_LINGER
      AC_CHECK_FUNCS(pmap_set, , [
	 AC_CHECK_LIB(rpc, pmap_set, [
	   LIBS="-lrpc $LIBS"
	  AC_DEFINE(HAVE_PMAP_SET)
	  ])])
      AC_CHECK_FUNCS(pmap_getport pmap_getmaps rresvport)
      dnl add for source routing support setsockopt
      AC_CHECK_HEADERS(rpc/pmap_clnt.h)
      vfs_flags="$vfs_flags, mcfs, ftpfs, fish"
      use_net_code=true
  fi

  dnl
  dnl Samba support
  dnl
  smbfs=""
  SAMBAFILES=""
  AC_ARG_WITH(samba,
  	  [--with-samba	            Support smb virtual file system],[
  	  if test "x$withval != xno"; then
  		  AC_DEFINE(WITH_SMBFS)
	          vfs_flags="$vfs_flags, smbfs"
		  smbfs="smbfs.o"
		  SAMBAFILES="\$(SAMBAFILES)"
  	  fi
  ])
  AC_SUBST(smbfs)
  AC_SUBST(SAMBAFILES)
  
  dnl
  dnl The termnet support
  dnl
  termnet=false
  AC_ARG_WITH(termnet,
	  [--with-termnet             If you want a termified net support],[
	  if test x$withval = xyes; then
		  AC_DEFINE(USE_TERMNET)
		  termnet=true		
	  fi
  ])

  TERMNET=""
  AC_DEFINE(USE_VFS)
  if $use_net_code; then
     AC_DEFINE(USE_NETCODE)
  fi
  mcserv=
  if test $have_socket = yes; then
     mcserv="mcserv"
     if $termnet; then
	TERMNET="-ltermnet"
     fi
  fi

  AC_SUBST(TERMNET)
  AC_SUBST(mcserv)

dnl FIXME:
dnl GNOME_VFS_LIBS=

])

AC_DEFUN([GNOME_VFS_CHECKS],[
	use_vfs=yes
	AC_ARG_WITH(vfs,
		[--with-vfs		   Compile with the VFS code],
		use_vfs=$withval
	)
	case $use_vfs in
		yes) 	GNOME_WITH_VFS;;
		no) 	use_vfs=no;;
		*)   	use_vfs=no;;
			dnl Should we issue a warning?
	esac
])


