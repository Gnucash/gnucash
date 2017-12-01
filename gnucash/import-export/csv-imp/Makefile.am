SUBDIRS = . gschemas test

pkglib_LTLIBRARIES=libgncmod-csv-import.la

libgncmod_csv_import_la_SOURCES = \
  gncmod-csv-import.c \
  assistant-csv-account-import.c \
  assistant-csv-trans-import.cpp \
  gnc-plugin-csv-import.c \
  csv-account-import.c \
  gnc-csv-account-map.c \
  gnc-csv-tokenizer.cpp \
  gnc-csv-gnumeric-popup.c \
  gnc-dummy-tokenizer.cpp \
  gnc-fw-tokenizer.cpp \
  gnc-price-import.cpp \
  gnc-price-props.cpp \
  gnc-tokenizer.cpp \
  gnc-tx-import.cpp \
  gnc-trans-props.cpp \
  gnc-csv-trans-settings.cpp

noinst_HEADERS = \
  assistant-csv-account-import.h \
  assistant-csv-trans-import.h \
  gnc-plugin-csv-import.h \
  csv-account-import.h \
  gnc-csv-account-map.h \
  gnc-csv-tokenizer.hpp \
  gnc-csv-gnumeric-popup.h \
  gnc-dummy-tokenizer.hpp \
  gnc-fw-tokenizer.hpp \
  gnc-price-import.hpp \
  gnc-price-props.hpp \
  gnc-tokenizer.hpp \
  gnc-tx-import.hpp \
  gnc-trans-props.hpp \
  gnc-csv-trans-settings.hpp

libgncmod_csv_import_la_LDFLAGS = -avoid-version

libgncmod_csv_import_la_LIBADD = \
  ${top_builddir}/gnucash/import-export/libgncmod-generic-import.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/borrowed/goffice/libgnc-goffice.la \
  ${top_builddir}/borrowed/libc/libc-missing.la \
  ${GLIB_LIBS} \
  ${GTK_LIBS} \
  ${ICU4C_I18N_LIBS} \
  ${BOOST_LDFLAGS} \
  -lboost_regex \
  -lboost_locale

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/import-export \
  -I${top_srcdir}/borrowed/libc \
  -I${top_srcdir}/borrowed/goffice \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${ICU4C_I18N_CFLAGS} \
  ${BOOST_CPPFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-csv-import-ui.xml

gtkbuilderdir = ${GNC_GTKBUILDER_DIR}
gtkbuilder_DATA = \
	assistant-csv-account-import.glade \
	assistant-csv-trans-import.glade

EXTRA_DIST = $(ui_DATA) $(gtkbuilder_DATA) CMakeLists.txt

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.import.csv\"
