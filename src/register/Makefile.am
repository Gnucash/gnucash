
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

CFLAGS = @CFLAGS@ @GNOME_CFLAGS@

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register/gnome


SUBDIRS = gnome
