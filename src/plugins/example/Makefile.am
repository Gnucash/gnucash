SUBDIRS = ui glade .

pkglib_LTLIBRARIES = libgncmod-example.la

libgncmod_example_la_SOURCES = \
  gnc-plugin-example.c \
  gncmod-example.c

noinst_HEADERS = \
  gnc-plugin-example.h

libgncmod_example_la_LDFLAGS = -avoid-version

libgncmod_example_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS} \
  ${AQBANKING_LIBS}

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
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${AQBANKING_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.plugin.example\"
