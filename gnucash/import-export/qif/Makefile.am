#SUBDIRS = . test
DIST_SUBDIRS = test

pkglib_LTLIBRARIES=libgncmod-qif.la

libgncmod_qif_la_SOURCES = \
  qif-context.c \
  qif-defaults.c \
  qif-file.c \
  qif-objects.c \
  qif-parse.c

noinst_HEADERS = \
  qif-file.h \
  qif-defaults.h \
  qif-import-p.h \
  qif-import.h \
  qif-objects.h \
  qif-objects-p.h \
  qif-parse.h

libgncmod_qif_la_LIBADD = \
  ${top_builddir}/gnucash/import-export/libgncmod-generic-import.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${REGEX_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/import-export \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.import.qif\"

EXTRA_DIST = CMakeLists.txt
