pkglib_LTLIBRARIES = libgncmod-ledger-core.la

libgncmod_ledger_core_la_SOURCES = \
  gncmod-ledger-core.c \
  MultiLedger.c \
  split-register.c \
  split-register-control.c \
  split-register-load.c \
  split-register-model.c \
  split-register-model-save.c \
  split-register-util.c

noinst_HEADERS = \
  MultiLedger.h \
  RegWindow.h \
  split-register.h \
  split-register-control.h \
  split-register-model.h \
  split-register-model-save.h \
  split-register-p.h

libgncmod_ledger_core_la_LDFLAGS = -module

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}

INCLUDES = \
   -I${top_srcdir}/src \
   -I${top_srcdir}/src/engine \
   -I${top_srcdir}/src/guile \
   -I${top_srcdir}/src/gnc-module \
   -I${top_srcdir}/src/app-utils \
   -I${top_srcdir}/src/register/register-core \
   ${GNOME_INCLUDEDIR}
