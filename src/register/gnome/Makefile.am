
noinst_LIBRARIES = libgncregistergnome.a

libgncregistergnome_a_SOURCES = \
  combocell-gnome.c \
  gnucash-color.c \
  gnucash-cursor.c \
  gnucash-grid.c \
  gnucash-header.c \
  gnucash-item-edit.c \
  gnucash-item-list.c \
  gnucash-sheet.c \
  gnucash-style.c \
  quickfillcell-gnome.c

CFLAGS = @CFLAGS@ @GNOME_CFLAGS@

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/guile \
  -I${top_srcdir}/src/register
