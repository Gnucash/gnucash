
SUBDIRS = \
  doc \
  gnc-module \
  test-core \
  network-utils \
  engine \
  backend \
  calculation \
  tax \
  app-utils \
  gnome-utils \
  gnome \
  app-file \
  register \
  import-export \
  report \
  experimental \
  scm \
  quotes \
  pixmaps \
  optional

# Engine Makefile.am file.
bin_PROGRAMS = gnucash

# libraries show up twice because they both provide functions for
# other libs and use functions from other libs.  These mutual
# dependencies need to go away.  The libgwrap one in particular is a
# problem with the current version of gwrap, and will go away when
# that's fixed.
gnucash_LDADD = \
  gnc-module/libgncmodule.la \
  gnome-utils/libgncmod-gnome-utils.la \
  app-utils/libgncmod-app-utils.la \
  app-file/libgncmod-app-file.la \
  engine/libgncmod-engine.la \
  engine/libgw-engine.la \
  engine/libgw-glib.la \
  backend/file/libgncmod-backend-file.la \
  register/ledger-core/libgncmod-ledger-core.la \
  register/register-core/libgncmod-register-core.la \
  register/register-gnome/libgncmod-register-gnome.la \
  report/report-system/libgncmod-report-system.la \
  import-export/qif-import/libgncmod-qif-import.la \
  gnome/libgncguile.la \
  gnome/libgncgnome.la \
  gnome/libgncguile.la \
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
  ${EFENCE_LIBS} \
  -lltdl

gnucash_SOURCES = \
  gnc-main.c

noinst_HEADERS = \
  AccWindow.h \
  RecnWindow.h \
  gnc-ui.h

EXTRA_DIST = \
  .cvsignore \
  README.modules \
  gnucash-make-guids.in \
  gnucash-run-script.in

AM_CFLAGS = \
  -I./app-utils \
  -I./engine \
  -I./guile \
  -I./register \
  -I./gnc-module \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}

bin_SCRIPTS = gnucash-run-script gnucash-make-guids

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
gnucash-run-script: gnucash-run-script.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GUILE-@:${GUILE}:' \
	    -e 's:@-GNC_GUILE_MODULE_DIR-@:${GNC_SHAREDIR}/guile-modules:' \
            -e 's:@-GNC_LIB_INSTALLDIR-@:${libdir}:' \
            -e 's:@-GNC_PKGLIB_INSTALLDIR-@:${pkglibdir}:' \
	    -e 's:@-GNC_SCM_INSTALL_DIR-@:${GNC_SCM_INSTALL_DIR}:' \
	    -e 's:@-G_WRAP_MODULE_DIR-@:${G_WRAP_MODULE_DIR}:'
	mv $@.tmp $@
BUILT_SOURCES = gnucash-run-script
CLEANFILES = gnucash-run-script

gnucash-make-guids: gnucash-make-guids.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GNUCASH_RUN_SCRIPT-@:${bindir}/gnucash-run-script:' \
	    -e 's:@-THIS_SCRIPT-@:${bindir}/$@:'
	mv $@.tmp $@
BUILT_SOURCES += gnucash-make-guids
CLEANFILES += gnucash-make-guids
