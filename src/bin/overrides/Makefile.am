#SUBDIRS = . test

gncoverridesdir = ${GNC_LIBEXECDIR}/overrides
# When you add a file here, please modify ../test/Makefile.am's overrides.stamp
gncoverrides_SCRIPTS = \
  gnucash \
  gnucash-env \
  gnucash-make-guids \
  gnucash-run-script \
  guile

EXTRA_DIST = \
  .cvsignore \
  gnucash-env.in \
  gnucash-build-env.in \
  guile.in 

noinst_DATA = gnucash-build-env

PWD := $(shell pwd)

## We borrow guile's convention and use @-...-@ as the substitution
## brackets below, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*

guile: guile.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GUILE-@:${GUILE}:'
	mv $@.tmp $@
	chmod u+x $@
BUILT_SOURCES = guile
CLEANFILES = guile

gnucash-env: gnucash-env.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-G_WRAP_MODULE_DIR-@:${G_WRAP_MODULE_DIR}:g' \
	    -e 's:@-GNC_GUILE_MODULE_DIR-@:${GNC_SHAREDIR}/guile-modules:g' \
	    -e 's:@-GNC_SCM_INSTALL_DIR-@:${GNC_SCM_INSTALL_DIR}:g' \
            -e 's:@-GNC_LIB_INSTALLDIR-@:${libdir}:' \
            -e 's:@-GNC_PKGLIB_INSTALLDIR-@:${pkglibdir}:g' \
	    -e 's:@-GNC_MODULE_DIR-@:${GNC_MODULE_DIR}:g' \
            -e 's:@-GNC_SCRIPT_OVERRIDE_DIR-@:${gncoverridedir}:g'
	mv $@.tmp $@
	chmod u+x $@
BUILT_SOURCES += gnucash-env
CLEANFILES += gnucash-env

gnucash-build-env: gnucash-build-env.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-G_WRAP_MODULE_DIR-@:${G_WRAP_MODULE_DIR}:g' \
            -e 's:@-GNC_BUILDDIR-@:${PWD}/${top_builddir}:g' \
            -e 's:@-GNC_SRCDIR-@:${PWD}/${top_srcdir}:g'
	mv $@.tmp $@
	chmod u+x $@
BUILT_SOURCES += gnucash-build-env
CLEANFILES += gnucash-build-env
