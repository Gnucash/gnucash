SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-register-gnome.la

libgncmod_register_gnome_la_LDFLAGS = -avoid-version

libgncmod_register_gnome_la_LIBADD = \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLIB_LIBS} \
  ${GNOME_LIBS}

libgncmod_register_gnome_la_SOURCES = \
  gncmod-register-gnome.c \
  combocell-gnome.c \
  datecell-gnome.c \
  formulacell-gnome.c \
  gnucash-color.c \
  gnucash-cursor.c \
  gnucash-date-picker.c \
  gnucash-grid.c \
  gnucash-header.c \
  gnucash-item-edit.c \
  gnucash-item-list.c \
  gnucash-scrolled-window.c \
  gnucash-sheet.c \
  gnucash-style.c \
  pricecell-gnome.c \
  quickfillcell-gnome.c \
  table-gnome.c 

noinst_HEADERS = \
  formulacell-gnome.h \
  gnucash-color.h \
  gnucash-cursor.h \
  gnucash-date-picker.h \
  gnucash-grid.h \
  gnucash-header.h \
  gnucash-item-edit.h \
  gnucash-item-list.h \
  gnucash-scrolled-window.h \
  gnucash-sheet.h \
  gnucash-style.h \
  pricecell-gnome.h \
  quickfillcell-gnome.h \
  table-gnome.h

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GTK_XIM_FLAGS} \
  ${GNOME_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.register.gnome\"
