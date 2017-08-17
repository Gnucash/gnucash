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
  ${top_builddir}/gnucash/import-export/libgncmod-generic-import.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/gnome/libgnc-gnome.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/borrowed/libc/libc-missing.la \
  ${GTK_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/register/ledger-core \
  -I${top_srcdir}/gnucash/register/register-core \
  -I${top_srcdir}/gnucash/register/register-gnome \
  -I${top_srcdir}/gnucash/import-export \
  -I${top_srcdir}/borrowed/libc \
  -I${top_srcdir}/lib \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  $(GTK_CFLAGS)

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-csv-export-ui.xml

gtkbuilderdir = ${GNC_GTKBUILDER_DIR}
gtkbuilder_DATA = \
	assistant-csv-export.glade

EXTRA_DIST = $(ui_DATA) $(gtkbuilder_DATA) CMakeLists.txt

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.export.csv\"
