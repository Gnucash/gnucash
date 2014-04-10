
noinst_LIBRARIES = libgncregister.a

libgncregister_a_SOURCES = \
  QuickFill.c \
  basiccell.c \
  cellblock.c \
  datecell.c \
  numcell.c \
  pricecell.c \
  quickfillcell.c \
  recncell.c \
  splitreg.c \
  table-allgui.c \
  table-gnome.c \
  table-html.c \
  textcell.c

noinst_HEADERS = \
  QuickFill.h \
  basiccell.h \
  cellblock.h \
  combocell.h \
  datecell.h \
  numcell.h \
  pricecell.h \
  quickfillcell.h \
  recncell.h \
  splitreg.h \
  table-allgui.h \
  table-gnome.h \
  table-html.h \
  textcell.h

EXTRA_DIST = \
  .cvsignore \
  design.txt

CFLAGS = @CFLAGS@ ${GNOME_CFLAGS} ${GUILE_COMPILE_ARGS}

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register/gnome


SUBDIRS = gnome
