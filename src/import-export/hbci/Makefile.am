SUBDIRS = . glade test

pkglib_LTLIBRARIES = libgncmod-hbci.la

libgncmod_hbci_la_SOURCES = \
  gncmod-hbci.c \
  druid-hbci-initial.c

noinst_HEADERS = 

libgncmod_hbci_la_LDFLAGS = -module

libgncmod_hbci_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${GLIB_LIBS} \
  ${GNOMEUI_LIBS} 

#  -L/home/chs/lib/unstable/lib -lopenhbci 

gncscmdir = ${GNC_SCM_INSTALL_DIR}/hbci

gncscm_DATA = \
	hbci.scm

#gladedir = ${GNC_GLADE_DIR}
#glade_DATA = 

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  ${GNOME_INCLUDEDIR} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS} 

#  -I/home/chs/lib/unstable/include

#gladedir = $(GNC_GLADE_DIR)
#glade_DATA = 

EXTRA_DIST = 

CLEANFILES = hbci import-export gnucash g-wrapped .scm-links
