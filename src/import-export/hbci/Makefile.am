SUBDIRS = . glade schemas test

pkglib_LTLIBRARIES = libgncmod-hbci.la

libgncmod_hbci_la_SOURCES = \
  dialog-daterange.c \
  dialog-hbcitrans.c \
  dialog-pass.c \
  gnc-file-aqb-import.c \
  gnc-hbci-getbalance.c \
  gnc-hbci-gettrans.c \
  gnc-hbci-kvp.c \
  gnc-hbci-trans-templ.c \
  gnc-hbci-transfer.c \
  gnc-hbci-utils.c \
  gnc-plugin-hbci.c \
  hbci-interaction.c \
  druid-hbci-utils.c \
  druid-hbci-initial.c \
  gncmod-hbci.c



# Must be included in tarball because they are listed in POTFILES.in
libgncmod_hbci_la_empty = \
  gnc-hbci-actions.c

noinst_HEADERS = \
  dialog-daterange.h \
  dialog-hbcitrans.h \
  dialog-pass.h \
  druid-hbci-initial.h \
  druid-hbci-utils.h \
  gnc-file-aqb-import.h \
  gnc-hbci-getbalance.h \
  gnc-hbci-gettrans.h \
  gnc-hbci-kvp.h \
  gnc-hbci-transfer.h \
  gnc-hbci-trans-templ.h \
  gnc-hbci-utils.h \
  gnc-plugin-hbci.h \
  hbci-interaction.h \
  hbci-interactionP.h

libgncmod_hbci_la_LDFLAGS = -avoid-version

libgncmod_hbci_la_LIBADD = \
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

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/register \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/ledger-core \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${AQBANKING_CFLAGS}

#gladedir = $(GNC_GLADE_DIR)
#glade_DATA = 

uidir = $(GNC_UI_DIR)
ui_DATA = \
	gnc-plugin-hbci-ui.xml

EXTRA_DIST = ${gncscm_DATA} ${libgncmod_hbci_la_empty} ${ui_DATA}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.import.hbci\"
