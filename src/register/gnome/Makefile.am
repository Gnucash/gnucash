
noinst_LIBRARIES = libgncregistergnome.a

libgncregistergnome_a_SOURCES = \
  combocell-gnome.c \
  datecell-gnome.c \
  gnucash-color.c \
  gnucash-cursor.c \
  gnucash-date-picker.c \
  gnucash-grid.c \
  gnucash-header.c \
  gnucash-item-edit.c \
  gnucash-item-list.c \
  gnucash-scrolled-window.c \
  gnucash-sheet.c \
  gnucash-style.c \
  quickfillcell-gnome.c \
  pricecell-gnome.c

noinst_HEADERS = \
  gnucash-color.h \
  gnucash-cursor.h \
  gnucash-date-picker.h \
  gnucash-grid.h \
  gnucash-header.h \
  gnucash-item-edit.h \
  gnucash-item-list.h \
  gnucash-scrolled-window.h \
  gnucash-sheet.h \
  gnucash-style.h

EXTRA_DIST = \
  .cvsignore

CFLAGS = @CFLAGS@ ${GTK_XIM_FLAGS} ${GNOME_INCLUDEDIR} ${GLIB_CFLAGS}

INCLUDES = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/register \
  ${GNOME_INCLUDEDIR}
