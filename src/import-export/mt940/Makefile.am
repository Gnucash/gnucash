SUBDIRS = . 
# test

pkglib_LTLIBRARIES=libgncmod-mt940.la

libgncmod_mt940_la_SOURCES = \
  gnc-mt940-import.c \
  gncmod-mt940-import.c \
  gnc-plugin-mt940.c

noinst_HEADERS = \
  gnc-mt940-import.h \
  gnc-plugin-mt940.h

libgncmod_mt940_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${GLIB_LIBS} \
  $(HBCI_LIBS)

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I$(top_srcdir)/src/import-export/hbci \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  $(HBCI_CFLAGS)

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-mt940-ui.xml

