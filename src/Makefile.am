
SUBDIRS = \
  doc \
  core-utils \
  gnc-module \
  test-core \
  network-utils \
  engine \
  backend \
  calculation \
  tax \
  app-utils \
  gnome-utils \
  app-file \
  import-export \
  report \
  dialog-account \
  register \
  gnome \
  experimental \
  scm \
  quotes \
  pixmaps \
  business \
  optional

# Engine Makefile.am file.
bin_PROGRAMS = gnucash
bin_SCRIPTS = gnucash-run-script gnucash-make-guids gnucash.sh

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
  ${GUILE_LIBS} \
  ${GUPPI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOMEUI_LIBS} \
  ${GNOME_PRINT_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GTKHTML_LIBS} \
  ${GHTTP_LIBS} \
  ${GLADE_LIBS} \
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
  gnucash-run-script.in \
  gnucash.sh.in

AM_CFLAGS = \
  -I./app-utils \
  -I./engine \
  -I./gnome \
  -I./register \
  -I./gnc-module \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}


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

gnucash.sh: gnucash.sh.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GNC_SHAREDIR-@:${GNC_SHAREDIR}:g' \
	    -e 's:@-G_WRAP_MODULE_DIR-@:${G_WRAP_MODULE_DIR}:g' \
	    -e 's:@-GNC_LIBDIR-@:${GNC_LIBDIR}:g' \
	    -e 's:@-GNC_MODULE_DIR-@:${GNC_MODULE_DIR}:g'
	mv $@.tmp $@
	chmod u+x $@
BUILT_SOURCES += gnucash.sh
CLEANFILES += gnucash.sh
