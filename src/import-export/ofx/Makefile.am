SUBDIRS = . test

pkglib_LTLIBRARIES=libgncmod-ofx.la

libgncmod_ofx_la_SOURCES = \
  gnc-ofx-import.c \
  gncmod-ofx-import.c

noinst_HEADERS = \
  gnc-ofx-import.h

libgncmod_ofx_la_LDFLAGS = -module

libgncmod_ofx_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${GLIB_LIBS} \
  -lofx -lstdc++

#gladedir = ${GNC_GLADE_DIR}
#glade_DATA = ofx.glade

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  ${GLIB_CFLAGS}

EXTRA_DIST = \
  .cvsignore 

CLEANFILES = ofx import-export gnucash g-wrapped .scm-links
