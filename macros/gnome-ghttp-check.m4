AC_DEFUN([GNOME_GHTTP_CHECK],[
	AC_REQUIRE([GNOME_INIT_HOOK])
	GHTTP_LIB=
	AC_CHECK_FUNC(connect,,[
	  AC_CHECK_LIB(socket,connect,
		GHTTP_LIB="-lsocket $GHTTP_LIB",,$GHTTP_LIB)])
	AC_CHECK_FUNC(gethostbyname,,[
 	  AC_CHECK_LIB(nsl,gethostbyname,
		GHTTP_LIB="-lnsl $GHTTP_LIB",,$GHTTP_LIB)])
	AC_CHECK_LIB(ghttp, ghttp_request_new, 
		GHTTP_LIB="-lghttp $GHTTP_LIB",GHTTP_LIB="",-L$gnome_prefix $GHTTP_LIB)
	AC_SUBST(GHTTP_LIB)
	AC_PROVIDE([GNOME_GHTTP_CHECK])
])
