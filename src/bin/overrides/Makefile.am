gncoverridesdir = ${GNC_LIBEXECDIR}/overrides
# When you add a file here, please modify ../test/Makefile.am's overrides.stamp
gncoverrides_SCRIPTS = \
  gnucash-env \
  gnucash-make-guids \
  guile

EXTRA_DIST = \
  gnucash-env.in \
  gnucash-build-env.in \
  gnucash-make-guids \
  guile.in 

noinst_DATA = gnucash-build-env

PWD := $(shell pwd)

## We borrow guile's convention and use @-...-@ as the substitution
## brackets below, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*

guile: guile.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-GUILE-@#${GUILE}#'
	mv $@.tmp $@
	chmod u+x $@
CLEANFILES = guile

gnucash-env: gnucash-env.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-GNC_SCM_INSTALL_DIR-@#${GNC_SCM_INSTALL_DIR}#g' \
            -e 's#@-GNC_LIB_INSTALLDIR-@#${libdir}#' \
            -e 's#@-GNC_PKGLIB_INSTALLDIR-@#${pkglibdir}#g' \
            -e 's#@-GNC_SCRIPT_OVERRIDE_DIR-@#${gncoverridedir}#g' \
            -e 's#@-GNC_PYTHON_DIR-@#${pythondir}#g'
	mv $@.tmp $@
	chmod u+x $@
CLEANFILES += gnucash-env

gnucash-build-env: gnucash-build-env.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's#@-GNC_BUILDDIR-@#${abs_top_builddir}#g' \
            -e 's#@-GNC_SRCDIR-@#${abs_top_srcdir}#g'
	mv $@.tmp $@
	chmod u+x $@
CLEANFILES += gnucash-build-env
