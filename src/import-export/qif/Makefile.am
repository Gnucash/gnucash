SUBDIRS = . test

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

libgncmod_qif_la_LDFLAGS = -module

libgncmod_qif_la_LIBADD =

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/import-export \
  ${GLIB_CFLAGS}
