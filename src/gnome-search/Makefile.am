pkglib_LTLIBRARIES = libgncmod-gnome-search.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/business/business-core \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
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
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${GTKHTML_LIBS} \
  ${GLADE_LIBS} \
  ${GLIB_LIBS} \
  ${REGEX_LIBS} \
  ${QOF_LIBS}

gladedir = $(GNC_GLADE_DIR)
glade_DATA = \
  search.glade

EXTRA_DIST = \
  ${glade_DATA}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.gui.search\"
