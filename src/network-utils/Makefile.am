# 2003-03-11 TomF changes for Gnome-2 branch, 5th batch
# Add src/engine to allow including gnc-engine-util.h

SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-network-utils.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  ${GLIB_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GUILE_INCS}

libgncmod_network_utils_la_SOURCES = \
  gnc-http.c \
  gncmod-network-utils.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-http.h

libgncmod_network_utils_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GNOME_LIBS} ${QOF_LIBS}

