SUBDIRS = . schemas

pkglib_LTLIBRARIES = libgncmod-aqbanking.la

libgncmod_aqbanking_la_SOURCES = \
  dialog-daterange.c \
  druid-ab-initial.c \
  gnc-ab-getbalance.c \
  gnc-ab-gettrans.c \
  gnc-ab-kvp.c \
  gnc-ab-utils.c \
  gnc-gwen-gui.c \
  gnc-plugin-aqbanking.c \
  gncmod-aqbanking.c

noinst_HEADERS = \
  dialog-daterange.h \
  druid-ab-initial.h \
  gnc-ab-getbalance.h \
  gnc-ab-gettrans.h \
  gnc-ab-kvp.h \
  gnc-ab-utils.h \
  gnc-gwen-gui.h \
  gnc-plugin-aqbanking.h

libgncmod_aqbanking_la_LDFLAGS = -avoid-version

libgncmod_aqbanking_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS} \
  ${AQBANKING_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/import-export \
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

uidir = $(GNC_UI_DIR)
ui_DATA = \
  gnc-plugin-aqbanking-ui.xml

gladedir = $(GNC_GLADE_DIR)
glade_DATA = \
  aqbanking.glade

EXTRA_DIST = ${ui_DATA} ${glade_DATA}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.import.aqbanking\"
