
noinst_LIBRARIES = libgncregister.a

libgncregister_a_SOURCES = \
  QuickFill.c \
  basiccell.c \
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
  textcell.c

noinst_HEADERS = \
  QuickFill.h \
  basiccell.h \
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
  textcell.h

EXTRA_DIST = \
  .cvsignore \
  table-html.c \
  table-html.h

CFLAGS = @CFLAGS@

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register/gnome \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}


SUBDIRS = gnome
