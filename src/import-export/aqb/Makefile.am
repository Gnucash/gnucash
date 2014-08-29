SUBDIRS = . gschemas test

pkglib_LTLIBRARIES = libgncmod-aqbanking.la

libgncmod_aqbanking_la_SOURCES = \
  dialog-ab-trans.c \
  dialog-ab-daterange.c \
  assistant-ab-initial.c \
  gnc-ab-getbalance.c \
  gnc-ab-gettrans.c \
  gnc-ab-kvp.c \
  gnc-ab-trans-templ.c \
  gnc-ab-transfer.c \
  gnc-ab-utils.c \
  gnc-file-aqb-import.c \
  gnc-gwen-gui.c \
  gnc-plugin-aqbanking.c \
  gncmod-aqbanking.c

noinst_HEADERS = \
  dialog-ab-trans.h \
  dialog-ab-daterange.h \
  assistant-ab-initial.h \
  gnc-ab-getbalance.h \
  gnc-ab-gettrans.h \
  gnc-ab-kvp.h \
  gnc-ab-trans-templ.h \
  gnc-ab-transfer.h \
  gnc-ab-utils.h \
  gnc-file-aqb-import.h \
  gnc-gwen-gui.h \
  gnc-plugin-aqbanking.h

libgncmod_aqbanking_la_LDFLAGS = -avoid-version
if OS_WIN32
libgncmod_aqbanking_la_LDFLAGS += -export-symbols-regex .*
endif
libgncmod_aqbanking_la_LIBADD = \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTK_LIBS} \
  ${GLIB_LIBS} \
  ${GWENGUI_GTK2_LIBS} \
  ${AQBANKING_LIBS}

AM_CPPFLAGS = \
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
  -I${top_srcdir}/src/libqof/qof \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${AQBANKING_CFLAGS}

uidir = $(GNC_UI_DIR)
ui_DATA = \
  gnc-plugin-aqbanking-ui.xml

gtkbuilderdir = $(GNC_GTKBUILDER_DIR)
gtkbuilder_DATA = \
  assistant-ab-initial.glade \
  dialog-ab.glade

EXTRA_DIST = ${ui_DATA} ${gtkbuilder_DATA} migratable-prefs-aqbanking.xml

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.import.aqbanking\"
