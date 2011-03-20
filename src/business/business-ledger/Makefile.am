SUBDIRS = .

lib_LTLIBRARIES = libgnc-business-ledger.la

libgnc_business_ledger_la_SOURCES = \
  gncEntryLedger.c \
  gncEntryLedgerControl.c \
  gncEntryLedgerDisplay.c \
  gncEntryLedgerLayout.c \
  gncEntryLedgerLoad.c \
  gncEntryLedgerModel.c

noinst_HEADERS = \
  gncEntryLedger.h \
  gncEntryLedgerP.h \
  gncEntryLedgerControl.h \
  gncEntryLedgerLayout.h \
  gncEntryLedgerModel.h

libgnc_business_ledger_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/business/business-utils/libgncmod-business-utils.la \
  ${top_builddir}/src/business/dialog-tax-table/libgncmod-dialog-tax-table.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTK_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src/business/business-core \
  -I${top_srcdir}/src/business/business-utils \
  -I${top_srcdir}/src/business/dialog-tax-table \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLIB_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.business.ledger\"
