SUBDIRS = .

pkglib_LTLIBRARIES = libgnc-business-ledger.la

libgnc_business_ledger_la_SOURCES = \
  gncEntryLedger.c \
  gncEntryLedgerControl.c \
  gncEntryLedgerLayout.c \
  gncEntryLedgerLoad.c \
  gncEntryLedgerModel.c

noinst_HEADERS = \
  gncEntryLedger.h \
  gncEntryLedgerP.h \
  gncEntryLedgerControl.h \
  gncEntryLedgerLayout.h \
  gncEntryLedgerModel.h

libgnc_business_ledger_la_LDFLAGS = -module
libgnc_business_ledger_la_LIBADD = \
  ${top_srcdir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_srcdir}/src/app-utils/libgncmod-app-utils.la \
  ${top_srcdir}/src/register/register-core/libgncmod-register-core.la \
  ${top_srcdir}/src/register/register-gnome/libgncmod-register-gnome.la

AM_CFLAGS = \
  -I../business-core \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/register/register-core \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GLIB_CFLAGS}
