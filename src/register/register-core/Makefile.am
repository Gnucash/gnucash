SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-register-core.la

libgncmod_register_core_la_LDFLAGS = -module

libgncmod_register_core_la_SOURCES = \
  gncmod-register-core.c \
  QuickFill.c \
  basiccell.c \
  cell-factory.c \
  cellblock.c \
  gtable.c \
  numcell.c \
  pricecell.c \
  quickfillcell.c \
  recncell.c \
  register-common.c \
  table-allgui.c \
  table-control.c \
  table-layout.c \
  table-model.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  QuickFill.h \
  basiccell.h \
  cell-factory.h \
  cellblock.h \
  combocell.h \
  datecell.h \
  gtable.h \
  numcell.h \
  pricecell.h \
  quickfillcell.h \
  recncell.h \
  register-common.h \
  table-allgui.h \
  table-control.h \
  table-layout.h \
  table-model.h

EXTRA_DIST = \
  .cvsignore 

CFLAGS = \
  @CFLAGS@ \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/guile \
  ${GLIB_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}
