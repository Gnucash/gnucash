dnl
dnl GNOME_XML2_HOOK (script-if-xml-found, failflag)
dnl
dnl If failflag is "failure", script aborts due to lack of XML
dnl 
dnl Check for availability of the libxml library
dnl the XML parser uses libz if available too
dnl

AC_DEFUN([GNOME_XML2_HOOK],[
	AC_PATH_PROG(PKG_CONFIG,pkg-config,no)
	if test "$PKG_CONFIG" = no; then
		if test x$2 = xfailure; then
			AC_MSG_ERROR(Could not find pkg-config)
		fi
	fi
	GNOME_XML_CFLAGS=`$PKG_CONFIG --cflags xml2`
	AS_SCRUB_INCLUDE(GNOME_XML_CFLAGS)
	AC_SUBST(GNOME_XML_CFLAGS)
	AC_CHECK_LIB(xml, xmlNewDoc, [
		$1
		GNOME_XML_LIB=`$PKG_CONFIG --libs xml2`
	], [
		if test x$2 = xfailure; then 
			AC_MSG_ERROR(Could not link sample xml program)
		fi
	], `$PKG_CONFIG --libs xml2`)
	AC_SUBST(GNOME_XML_LIB)
])

AC_DEFUN([GNOME_XML2_CHECK], [
	GNOME_XML2_HOOK([],failure)
])
