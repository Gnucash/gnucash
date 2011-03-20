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
  split-register-util.c

noinst_HEADERS = \
  gnc-ledger-display.h \
  split-register.h \
  split-register-control.h \
  split-register-layout.h \
  split-register-model.h \
  split-register-model-save.h \
  split-register-p.h

libgncmod_ledger_core_la_LDFLAGS = -avoid-version

libgncmod_ledger_core_la_LIBADD = \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLADE_LIBS} \
  ${GCONF_LIBS} \
  ${GUILE_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLIB_CFLAGS}


INCLUDES = -DG_LOG_DOMAIN=\"gnc.register.ledger\"
