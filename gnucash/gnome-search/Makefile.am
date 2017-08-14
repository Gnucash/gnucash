pkglib_LTLIBRARIES = libgncmod-gnome-search.la

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  ${GUILE_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS}

libgncmod_gnome_search_la_SOURCES = \
  gncmod-gnome-search.c \
  gnc-general-search.c \
  dialog-search.c \
  search-account.c \
  search-boolean.c \
  search-core-type.c \
  search-core-utils.c \
  search-date.c \
  search-double.c \
  search-int64.c \
  search-numeric.c \
  search-reconciled.c \
  search-string.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  dialog-search.h \
  gnc-general-search.h

noinst_HEADERS = \
  search-account.h \
  search-boolean.h \
  search-core-type.h \
  search-core-utils.h \
  search-date.h \
  search-double.h \
  search-int64.h \
  search-numeric.h \
  search-reconciled.h \
  search-string.h

libgncmod_gnome_search_la_LDFLAGS = -avoid-version

libgncmod_gnome_search_la_LIBADD = \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${REGEX_LIBS}

gtkbuilderdir = $(GNC_GTKBUILDER_DIR)
gtkbuilder_DATA = \
  dialog-search.glade

EXTRA_DIST = \
  ${gtkbuilder_DATA} \
  CMakeLists.txt

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.gui.search\"
