lib_LTLIBRARIES=libgncmod-ledger-core.la

libgncmod_ledger_core_la_SOURCES=\
  gncmod-ledger-core.c \
  SplitLedger.c \
  MultiLedger.c

libgncmod_ledger_core_la_LDFLAGS=-module -L${top_srcdir}/src/engine \
  -L${top_srcdir}/src/engine/.libs -lgncmod-engine

CFLAGS=@CFLAGS@

INCLUDES= \
   -I${top_srcdir}/src \
   -I${top_srcdir}/src/engine \
   -I${top_srcdir}/src/guile \
   -I${top_srcdir}/src/gnc-module \
   -I${top_srcdir}/src/register/register-core \
   ${GNOME_INCLUDEDIR} \
   ${GLIB_CFLAGS}
