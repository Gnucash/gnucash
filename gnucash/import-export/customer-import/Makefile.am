SUBDIRS = ui gtkbuilder .

pkglib_LTLIBRARIES = libgncmod-customer-import.la

libgncmod_customer_import_la_SOURCES = \
  gnc-plugin-customer-import.c \
  gncmod-customer-import.c \
  dialog-customer-import-gui.c \
  dialog-customer-import.c

noinst_HEADERS = \
  gnc-plugin-customer-import.h \
  dialog-customer-import-gui.h \
  dialog-customer-import.h

libgncmod_customer_import_la_LDFLAGS = -avoid-version

libgncmod_customer_import_la_LIBADD = \
  ${top_builddir}/gnucash/gnome/libgnc-gnome.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_builddir}/common \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/report/report-gnome \
  -I${top_srcdir}/gnucash/gnome-search/ \
  -I${top_srcdir}/gnucash/register/ledger-core \
  -I${top_srcdir}/gnucash/register/register-gnome \
  -I${top_srcdir}/gnucash/register/register-core \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/lib/libc \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.plugin.customer-import\"

EXTRA_DIST = CMakeLists.txt
