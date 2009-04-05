SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-binary-import.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnome \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GNOME_CFLAGS}

#FIXME remove the following deps
#  -I${top_srcdir}/src \
#  -I${top_srcdir}/src/gnome

libgncmod_binary_import_la_SOURCES = \
  druid-commodity.c \
  gncmod-binary-import.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  druid-commodity.h

libgncmod_binary_import_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${GUILE_LIBS} \
  ${GLADE_LIBS} \
  ${GNOME_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

gladedir = $(GNC_GLADE_DIR)
glade_DATA = binary-import.glade

EXTRA_DIST = \
  ${glade_DATA}
