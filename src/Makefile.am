
SUBDIRS = \
  doc \
  engine \
  calculation \
  experimental \
  register \
  guile \
  gnome \
  scm \
  quotes \
  pixmaps \
  optional \
  test

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
  calculation/libgnccalc.a \
  engine/libgncengine.la \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_PRINT_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GTKHTML_LIBS} \
  ${GHTTP_LIBS} \
  ${GUPPI_LIBS} \
  ${DB_LIBS} \
  ${INTLLIBS} \
  ${EFENCE_LIBS}

gnucash_SOURCES = \
  MultiLedger.c \
  SplitLedger.c \
  EuroUtils.c \
  FileDialog.c \
  gnc-component-manager.c \
  gnc-exp-parser.c \
  gnc-ui-util.c

noinst_HEADERS = \
  AccWindow.h \
  EuroUtils.h \
  FileBox.h \
  FileDialog.h \
  MainWindow.h \
  MultiLedger.h \
  RecnWindow.h \
  RegWindow.h \
  SplitLedger.h \
  file-history.h \
  gnc-component-manager.h \
  gnc-dir.h \
  gnc-exp-parser.h \
  gnc-ui.h \
  gnc-ui-common.h \
  gnc-ui-util.h \
  messages.h

EXTRA_DIST = \
  .cvsignore \
  gnc-dir.h.in

CFLAGS = @CFLAGS@ ${GNOME_INCLUDEDIR} ${GUILE_INCS}

INCLUDES = \
  -I./calculation \
  -I./engine \
  -I./guile \
  -I./register

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-dir.h: gnc-dir.h.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GNC_PIXMAP_DIR-@:${GNC_PIXMAP_DIR}:g'
	mv $@.tmp $@
BUILT_SOURCES += gnc-dir.h

# We have to do this because otherwise automake insists on putting
# these files into the dist tarfile.  If there's a a better way,
# by all means, let us know...
dist-hook:
	rm ${distdir}/gnc-dir.h
