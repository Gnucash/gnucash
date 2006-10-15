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
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${QOF_LIBS} \
  ${REGEX_LIBS} \
  ${GLIB_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/import-export \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS}

