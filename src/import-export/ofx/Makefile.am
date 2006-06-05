if !HAVE_GLIB26
  GLIB26_LIB = ${top_builddir}/lib/glib26/libgncglib.la
  GLIB26_INC = -I${top_srcdir}/lib/glib26
endif

SUBDIRS = . test

pkglib_LTLIBRARIES=libgncmod-ofx.la

libgncmod_ofx_la_SOURCES = \
  gnc-ofx-import.c \
  gncmod-ofx-import.c \
  gnc-plugin-ofx.c

noinst_HEADERS = \
  gnc-ofx-import.h \
  gnc-plugin-ofx.h

libgncmod_ofx_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${GLIB26_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS} \
  ${LIBOFX_LIBS}

AM_CFLAGS = \
  ${GLIB26_INC} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${LIBOFX_CFLAGS} 

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-ofx-ui.xml

EXTRA_DIST = $(ui_DATA)
