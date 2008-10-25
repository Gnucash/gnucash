SUBDIRS = .

pkglib_LTLIBRARIES=libgncmod-csv.la

libgncmod_csv_la_SOURCES = \
  gncmod-csv-import.c \
  gnc-plugin-csv.c \
  gnc-csv-import.c \
  gnc-csv-model.c \
  gnc-csv-gnumeric-popup.c

noinst_HEADERS = \
  gnc-plugin-csv.h \
  gnc-csv-import.h \
  gnc-csv-model.h \
  gnc-csv-gnumeric-popup.h

libgncmod_csv_la_LDFLAGS = -avoid-version

libgncmod_csv_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/lib/stf/libgnc-stf.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${QOF_LIBS} \
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
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/lib \
  ${QOF_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  $(GOFFICE_CFLAGS)

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-csv-ui.xml

gladedir = ${GNC_GLADE_DIR}
glade_DATA = \
	gnc-csv-preview-dialog.glade

EXTRA_DIST = $(ui_DATA) $(glade_DATA)

INCLUDES = -DG_LOG_DOMAIN=\"gnc.import.csv\"
