SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-ledger-core.la

libgncmod_ledger_core_la_SOURCES = \
  dialog-dup-trans.c \
  gnc-ledger-display.c \
  gncmod-ledger-core.c \
  split-register.c \
  split-register-control.c \
  split-register-layout.c \
  split-register-load.c \
  split-register-model.c \
  split-register-model-save.c \
  split-register-util.c \
  gnc-regwidget.c

noinst_HEADERS = \
  gnc-ledger-display.h \
  split-register.h \
  split-register-control.h \
  split-register-layout.h \
  split-register-model.h \
  split-register-model-save.h \
  split-register-p.h \
  gnc-regwidget.h

libgncmod_ledger_core_la_LDFLAGS = -module
libgncmod_ledger_core_la_LIBADD = \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/dialog-account/libdialog-account.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GLIB_CFLAGS}
