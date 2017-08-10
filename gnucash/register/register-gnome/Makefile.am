SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-register-gnome.la

libgncmod_register_gnome_la_LDFLAGS = -avoid-version

libgncmod_register_gnome_la_LIBADD = \
  ${top_builddir}/gnucash/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${GLIB_LIBS} \
  ${GTK_LIBS}

libgncmod_register_gnome_la_SOURCES = \
  gncmod-register-gnome.c \
  combocell-gnome.c \
  datecell-gnome.c \
  formulacell-gnome.c \
  gnucash-color.c \
  gnucash-cursor.c \
  gnucash-date-picker.c \
  gnucash-header.c \
  gnucash-item-edit.c \
  gnucash-item-list.c \
  gnucash-register.c \
  gnucash-scrolled-window.c \
  gnucash-sheet-private.c \
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
  gnucash-header.h \
  gnucash-item-edit.h \
  gnucash-item-list.h \
  gnucash-register.h \
  gnucash-scrolled-window.h \
  gnucash-sheet.h \
  gnucash-sheetP.h \
  gnucash-style.h \
  pricecell-gnome.h \
  quickfillcell-gnome.h \
  table-gnome.h

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/register/ledger-core \
  -I${top_srcdir}/gnucash/register/register-core \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GTK_XIM_FLAGS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.register.gnome\"

EXTRA_DIST = CMakeLists.txt
