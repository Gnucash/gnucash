
bin_SCRIPTS = gnucash-env gnucash gnucash-run-script gnucash-make-guids

gncoverridedir = ${GNC_LIBEXECDIR}/script-override
gncoverride_SCRIPTS = guile

EXTRA_DIST = \
  .cvsignore \
  gnucash-env \
  gnucash.in \
  guile.in \
  gnucash-make-guids.in \
  gnucash-run-script.in

## We borrow guile's convention and use @-...-@ as the substitution
## brackets below, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*

guile: guile.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GUILE-@:${GUILE}:'
	mv $@.tmp $@
BUILT_SOURCES = guile
CLEANFILES = guile

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

gnucash-env: gnucash-env.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GUILE-@:${GUILE}:g' \
	    -e 's:@-G_WRAP_MODULE_DIR-@:${G_WRAP_MODULE_DIR}:g' \
	    -e 's:@-GNC_GUILE_MODULE_DIR-@:${GNC_SHAREDIR}/guile-modules:g' \
	    -e 's:@-GNC_SCM_INSTALL_DIR-@:${GNC_SCM_INSTALL_DIR}:g' \
	    -e 's:@-GNC_LIBDIR-@:${GNC_LIBDIR}:g' \
	    -e 's:@-GNC_MODULE_DIR-@:${GNC_MODULE_DIR}:g' \
            -e 's:@-GNC_PKGLIB_INSTALLDIR-@:${pkglibdir}:g' \
            -e 's:@-GNC_SCRIPT_OVERRIDE_DIR-@:${gncoverridedir}:g'
	mv $@.tmp $@
	chmod u+x $@
BUILT_SOURCES += gnucash-env
CLEANFILES += gnucash-env

gnucash: gnucash.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GNC_SHAREDIR-@:${GNC_SHAREDIR}:g' \
	    -e 's:@-G_WRAP_MODULE_DIR-@:${G_WRAP_MODULE_DIR}:g' \
	    -e 's:@-GNC_LIBDIR-@:${GNC_LIBDIR}:g' \
	    -e 's:@-GNC_MODULE_DIR-@:${GNC_MODULE_DIR}:g'
	mv $@.tmp $@
	chmod u+x $@
BUILT_SOURCES += gnucash
CLEANFILES += gnucash
