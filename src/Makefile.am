
SUBDIRS = \
  doc \
  gnc-module \
  engine \
  backend \
  app-utils \
  register \
  import-export \
  report \
  calculation \
  experimental \
  guile \
  gnome \
  scm \
  quotes \
  pixmaps \
  optional \
  . \
  test

# Engine Makefile.am file.
bin_PROGRAMS = gnucash gnucash-make-guids

# libraries show up twice because they both provide functions for
# other libs and use functions from other libs.  These mutual
# dependencies need to go away.  The libgwrap one in particular is a
# problem with the current version of gwrap, and will go away when
# that's fixed.
gnucash_LDADD = \
  -Lengine -Lengine/.libs \
  -Lgnc-module -Lgnc-module/.libs \
  gnc-module/libgncmodule.la \
  app-utils/libgncmod-app-utils.la \
  engine/libgncmod-engine.la \
  engine/libgw-engine.la \
  engine/libgw-glib.la \
  backend/file/libgncmod-backend-file.la \
  register/ledger-core/libgncmod-ledger-core.la \
  register/register-core/libgncmod-register-core.la \
  register/register-gnome/libgncmod-register-gnome.la \
  import-export/qif-import/libgncmod-qif-import.la \
  guile/libgncguile.a \
  gnome/libgncgnome.a \
  calculation/libgnccalc.a \
  guile/libgncguile.a \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_PRINT_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GTKHTML_LIBS} \
  ${GHTTP_LIBS} \
  ${GLADE_LIBS} \
  ${GUPPI_LIBS} \
  ${DB_LIBS} \
  ${INTLLIBS} \
  ${EFENCE_LIBS}

gnucash_SOURCES = \
  EuroUtils.c \
  FileDialog.c \
  gnc-main.c \
  gnc-exp-parser.c 

gnucash_make_guids_LDADD = \
  gnc-module/libgncmodule.la \
  engine/libgncmod-engine.la \
  ${DL_LIB} \
  ${GNOME_XML_LIB} \
  ${GLIB_LIBS} \
  ${INTLLIBS}

gnucash_make_guids_SOURCES = \
  gnucash-make-guids.c

noinst_HEADERS = \
  AccWindow.h \
  EuroUtils.h \
  FileBox.h \
  FileDialog.h \
  RecnWindow.h \
  file-history.h \
  gnc-ui.h \
  gnc-ui-common.h \
  messages.h

EXTRA_DIST = \
  .cvsignore \
  README.modules

CFLAGS = @CFLAGS@ ${GNOME_INCLUDEDIR} ${GUILE_INCS}

INCLUDES = \
  -I./app-utils \
  -I./calculation \
  -I./engine \
  -I./guile \
  -I./register \
  -I./gnc-module

