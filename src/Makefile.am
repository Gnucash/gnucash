
SUBDIRS = engine register guile gnome scm quotes pixmaps optional

# Engine Makefile.am file.
bin_PROGRAMS = gnucash

# libraries show up twice because they both provide functions for
# other libs and use functions from other libs.  These mutual
# dependencies need to go away.  The libgwrap one in particular is a
# problem with the current version of gwrap, and will go away when
# that's fixed.
LDADD = \
  gnome/libgncgnome.a \
  register/libgncregister.a \
  register/gnome/libgncregistergnome.a \
  guile/libgncguile.a \
  gnome/libgncgnome.a \
  engine/libgncengine.la \
  @GNOME_LIBS@ \
  @G_WRAP_LINK_ARGS@ \
  @GUILE_LINK_ARGS@

gnucash_SOURCES = \
  MultiLedger.c \
  SplitLedger.c \
  Destroy.c \
  EuroUtils.c \
  FileDialog.c \
  Refresh.c

CFLAGS = @CFLAGS@ @GNOME_CFLAGS@

INCLUDES = \
  -I./engine \
  -I./guile \
  -I./register
