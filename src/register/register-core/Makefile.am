SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-register-core.la

libgncmod_register_core_la_LDFLAGS = -avoid-version

libgncmod_register_core_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

libgncmod_register_core_la_SOURCES = \
  gncmod-register-core.c \
  basiccell.c \
  cell-factory.c \
  cellblock.c \
  formulacell.c \
  gtable.c \
  numcell.c \
  pricecell.c \
  quickfillcell.c \
  recncell.c \
  checkboxcell.c \
  register-common.c \
  table-allgui.c \
  table-control.c \
  table-layout.c \
  table-model.c


gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  basiccell.h \
  cell-factory.h \
  cellblock.h \
  combocell.h \
  datecell.h \
  formulacell.h \
  gtable.h \
  numcell.h \
  pricecell.h \
  quickfillcell.h \
  recncell.h \
  checkboxcell.h \
  register-common.h \
  table-allgui.h \
  table-control.h \
  table-layout.h \
  table-model.h

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLIB_CFLAGS}

CLEANFILES = $(BUILT_SOURCES)

INCLUDES = -DG_LOG_DOMAIN=\"gnc.register.core\"
