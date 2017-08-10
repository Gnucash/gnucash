SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-ledger-core.la

libgncmod_ledger_core_la_SOURCES = \
  gnc-ledger-display.c \
  gnc-ledger-display2.c \
  gncmod-ledger-core.c \
  split-register.c \
  split-register-control.c \
  split-register-layout.c \
  split-register-load.c \
  split-register-model.c \
  split-register-model-save.c \
  split-register-util.c \
  gncEntryLedger.c \
  gncEntryLedgerControl.c \
  gncEntryLedgerDisplay.c \
  gncEntryLedgerLayout.c \
  gncEntryLedgerLoad.c \
  gncEntryLedgerModel.c

noinst_HEADERS = \
  gnc-ledger-display.h \
  gnc-ledger-display2.h \
  split-register.h \
  split-register-control.h \
  split-register-layout.h \
  split-register-model.h \
  split-register-model-save.h \
  split-register-p.h \
  gncEntryLedger.h \
  gncEntryLedgerP.h \
  gncEntryLedgerControl.h \
  gncEntryLedgerLayout.h \
  gncEntryLedgerModel.h

libgncmod_ledger_core_la_LDFLAGS = -avoid-version

libgncmod_ledger_core_la_LIBADD = \
  ${top_builddir}/gnucash/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/gnucash/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${GUILE_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_builddir}/common \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_builddir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/register/register-core \
  -I${top_srcdir}/gnucash/register/register-gnome \
  ${GUILE_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GNOME_CFLAGS}


AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.register.ledger\"

EXTRA_DIST = CMakeLists.txt
