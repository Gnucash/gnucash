SUBDIRS = . glade test

pkglib_LTLIBRARIES = libgncmod-hbci.la

libgncmod_hbci_la_SOURCES = \
  dialog-daterange.c \
  dialog-hbcitrans.c \
  dialog-pass.c \
  gnc-hbci-cb.c \
  gnc-hbci-getbalance.c \
  gnc-hbci-gettrans.c \
  gnc-hbci-kvp.c \
  gnc-hbci-trans-templ.c \
  gnc-hbci-transfer.c \
  gnc-hbci-utils.c \
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
  gnc-hbci-cb.h \
  gnc-hbci-getbalance.h \
  gnc-hbci-gettrans.h \
  gnc-hbci-kvp.h \
  gnc-hbci-transfer.h \
  gnc-hbci-trans-templ.h \
  gnc-hbci-utils.h \
  hbci-interaction.h \
  hbci-interactionP.h


libgncmod_hbci_la_LDFLAGS = -module

libgncmod_hbci_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome/libgncgnome.la \
  ${GLADE_LIBS} \
  ${GLIB_LIBS} \
  ${GNOMEUI_LIBS} \
  ${HBCI_LIBS}

gncscmdir = ${GNC_SCM_INSTALL_DIR}/hbci

gncscm_DATA = \
  hbci.scm

#gladedir = ${GNC_GLADE_DIR}
#glade_DATA = 

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/register \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/ledger-core \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GTKHTML_CFLAGS} \
  ${HBCI_CFLAGS}

#gladedir = $(GNC_GLADE_DIR)
#glade_DATA = 

EXTRA_DIST = ${gncscm_DATA} ${libgncmod_hbci_la_empty}

CLEANFILES = hbci import-export gnucash g-wrapped .scm-links
