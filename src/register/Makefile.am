SUBDIRS = gnome

noinst_LIBRARIES = libgncregister.a

libgncregister_a_SOURCES = \
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
  splitreg.c \
  table-allgui.c \
  table-gnome.c \
  table-model.c \
  textcell.c

noinst_HEADERS = \
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
  splitreg.h \
  table-allgui.h \
  table-model.h \
  textcell.h

EXTRA_DIST = \
  .cvsignore

CFLAGS = @CFLAGS@

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register/gnome \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}
