SUBDIRS = . 
# test

pkglib_LTLIBRARIES=libgncmod-mt940.la

libgncmod_mt940_la_SOURCES = \
  gnc-mt940-import.c \
  gncmod-mt940-import.c

noinst_HEADERS = \
  gnc-mt940-import.h

libgncmod_mt940_la_LDFLAGS = -module

libgncmod_mt940_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${GLIB_LIBS} \
  $(HBCI_LIBS)

gncscmdir = ${GNC_SCM_INSTALL_DIR}/mt940

gncscm_DATA = \
	mt940-import.scm

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I$(top_srcdir)/src/import-export/hbci \
  ${GNOME_INCLUDEDIR} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  $(HBCI_CFLAGS)

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA}

CLEANFILES = mt940 import-export gnucash g-wrapped .scm-links
