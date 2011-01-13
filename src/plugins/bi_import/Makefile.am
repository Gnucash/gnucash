SUBDIRS = ui glade .

pkglib_LTLIBRARIES = libgncmod-bi_import.la

libgncmod_bi_import_la_SOURCES = \
  gnc-plugin-bi_import.c \
  gncmod-bi_import.c \
  gui.c \
  helpers.c \
  bi_import.c

noinst_HEADERS = \
  gnc-plugin-bi_import.h \
  gui.h \
  helpers.h \
  bi_import.h

libgncmod_bi_import_la_LDFLAGS = -avoid-version

libgncmod_bi_import_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/lib/libc \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.plugin.bi_import\"
