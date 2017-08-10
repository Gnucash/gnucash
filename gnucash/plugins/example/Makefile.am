SUBDIRS = ui glade .

pkglib_LTLIBRARIES = libgncmod-example.la

libgncmod_example_la_SOURCES = \
  gnc-plugin-example.c \
  gncmod-example.c

noinst_HEADERS = \
  gnc-plugin-example.h

libgncmod_example_la_LDFLAGS = -avoid-version

libgncmod_example_la_LIBADD = \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${GLIB_LIBS} \
  ${AQBANKING_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/register/ledger-core \
  -I${top_srcdir}/gnucash/register/register-gnome \
  -I${top_srcdir}/gnucash/register/register-core \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${AQBANKING_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.plugin.example\"
