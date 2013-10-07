SUBDIRS = ui gtkbuilder .

pkglib_LTLIBRARIES = libgncmod-bi_import.la

libgncmod_bi_import_la_SOURCES = \
  gnc-plugin-bi-import.c \
  gncmod-bi-import.c \
  dialog-bi-import-gui.c \
  dialog-bi-import-helper.c \
  dialog-bi-import.c

noinst_HEADERS = \
  gnc-plugin-bi-import.h \
  dialog-bi-import-gui.h \
  dialog-bi-import-helper.h \
  dialog-bi-import.h

libgncmod_bi_import_la_LDFLAGS = -avoid-version

libgncmod_bi_import_la_LIBADD = \
  ${top_builddir}/src/business/business-gnome/libgncmod-business-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/business/business-gnome/ \
  -I${top_srcdir}/src/report/report-gnome \
  -I${top_srcdir}/src/gnome-search/ \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/business/business-ledger/ \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/business/dialog-tax-table \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/lib/libc \
  ${GTK_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.plugin.bi-import\"
