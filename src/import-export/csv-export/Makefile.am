SUBDIRS = . gschemas

pkglib_LTLIBRARIES=libgncmod-csv-export.la

libgncmod_csv_export_la_SOURCES = \
  gncmod-csv-export.c \
  gnc-plugin-csv-export.c \
  assistant-csv-export.c \
  csv-tree-export.c \
  csv-transactions-export.c

noinst_HEADERS = \
  gnc-plugin-csv-export.h \
  assistant-csv-export.h \
  csv-tree-export.h \
  csv-transactions-export.h

libgncmod_csv_export_la_LDFLAGS = -avoid-version

libgncmod_csv_export_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/lib/stf/libgnc-stf.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GOFFICE_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/lib \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  $(GOFFICE_CFLAGS)

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-csv-export-ui.xml

gtkbuilderdir = ${GNC_GTKBUILDER_DIR}
gtkbuilder_DATA = \
	assistant-csv-export.glade

EXTRA_DIST = $(ui_DATA) $(gtkbuilder_DATA)

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.export.csv\"
