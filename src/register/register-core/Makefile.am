SUBDIRS = . test

pkglib_LTLIBRARIES = libgnc-register-core.la

libgnc_register_core_la_LDFLAGS = -avoid-version

libgnc_register_core_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${GUILE_LIBS} \
  ${QOF_LIBS} ${GLIB_LIBS}

libgnc_register_core_la_SOURCES = \
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

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}

CLEANFILES = $(BUILT_SOURCES)

INCLUDES = -DG_LOG_DOMAIN=\"gnc.register.core\"
