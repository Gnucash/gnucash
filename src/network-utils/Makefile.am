SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-network-utils.la

CFLAGS = \
  @CFLAGS@ \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} \
  ${GHTTP_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}

libgncmod_network_utils_la_SOURCES = \
  gnc-gpg.c \
  gnc-http.c \
  gncmod-network-utils.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-gpg.h \
  gnc-http.h

libgncmod_network_utils_la_LDFLAGS = -module

libgncmod_network_utils_la_LIBADD = \
  -L../gnc-module -L../gnc-module/.libs -lgncmodule \
  ${GHTTP_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_LIBS} \
  ${GNOMEUI_LIBS}

EXTRA_DIST = \
  .cvsignore
