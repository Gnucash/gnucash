SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-network-utils.la

CFLAGS = @CFLAGS@ \
         ${GLIB_CFLAGS} \
	 ${GHTTP_CFLAGS} \
         ${GNOME_INCLUDEDIR}

INCLUDES = \
  -I${top_srcdir}/src/gnc-module

libgncmod_network_utils_la_SOURCES = \
  gnc-gpg.c \
  gnc-http.c \
  gncmod-network-utils.c

noinst_HEADERS = \
  gnc-gpg.c \
  gnc-http.c

libgncmod_network_utils_la_LDFLAGS = -module

libgncmod_network_utils_la_LIBADD = \
  -L../gnc-module -L../gnc-module/.libs -lgncmodule \
  ${GHTTP_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_LIBS} \
  ${GNOMEUI_LIBS}

EXTRA_DIST = \
  .cvsignore
