# Order is important here.
SUBDIRS = . overrides test

AM_CPPFLAGS = -I${top_builddir} ${GLIB_CFLAGS} ${GNOME_CFLAGS} ${GTK_CFLAGS} \
  -DPKGSYSCONFDIR=\"${GNC_CONFIGDIR}\" \
  -DPKGDATADIR=\"${GNC_SHAREDIR}\" \
  -DGNC_HELPDIR=\"${GNC_HELPDIR}\" \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnome \
  -I${top_builddir}/src/gnome-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS}

bin_PROGRAMS = gnucash-bin
gnucash_bin_SOURCES = gnucash-bin.c
gnucash_bin_LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${GNOME_LIBS} \
  ${GTK_LIBS} \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la

gnucash: gnucash.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-BIN_DIR-@#${bindir}#g' \
		-e 's#@-GNC_DBD_DIR-@#${GNC_DBD_DIR}#g' \
	    -e 's#@-GNC_GUILE_MODULE_DIR-@#${GNC_SHAREDIR}/guile-modules#g' \
	    -e 's#@-GNC_SCM_INSTALL_DIR-@#${GNC_SCM_INSTALL_DIR}#g' \
        -e 's#@-GNC_LIB_INSTALLDIR-@#${libdir}#' \
        -e 's#@-GNC_PKGLIB_INSTALLDIR-@#${pkglibdir}#g' \
	    -e 's#@-PATH_SEPARATOR-@#${PATH_SEPARATOR}#g'
	mv $@.tmp $@
	chmod u+x $@

gnucash-gdb: gnucash-gdb.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-BIN_DIR-@#${bindir}#g' \
		-e 's#@-GNC_DBD_DIR-@#${GNC_DBD_DIR}#g' \
	    -e 's#@-GNC_GUILE_MODULE_DIR-@#${GNC_SHAREDIR}/guile-modules#g' \
	    -e 's#@-GNC_SCM_INSTALL_DIR-@#${GNC_SCM_INSTALL_DIR}#g' \
        -e 's#@-GNC_LIB_INSTALLDIR-@#${libdir}#' \
        -e 's#@-GNC_PKGLIB_INSTALLDIR-@#${pkglibdir}#g' \
	    -e "s#@-TOP_SRC_DIR-@#`pwd`/${top_srcdir}#g" \
	    -e 's#@-PATH_SEPARATOR-@#${PATH_SEPARATOR}#g'
	mv $@.tmp $@
	chmod u+x $@

gnucash-valgrind: gnucash-valgrind.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-BIN_DIR-@#${bindir}#g' \
		-e 's#@-GNC_DBD_DIR-@#${GNC_DBD_DIR}#g' \
	    -e 's#@-GNC_GUILE_MODULE_DIR-@#${GNC_SHAREDIR}/guile-modules#g' \
	    -e 's#@-GNC_SCM_INSTALL_DIR-@#${GNC_SCM_INSTALL_DIR}#g' \
        -e 's#@-GNC_LIB_INSTALLDIR-@#${libdir}#' \
        -e 's#@-GNC_PKGLIB_INSTALLDIR-@#${pkglibdir}#g' \
	    -e "s#@-TOP_SRC_DIR-@#`pwd`/${top_srcdir}#g" \
	    -e 's#@-PATH_SEPARATOR-@#${PATH_SEPARATOR}#g'
	mv $@.tmp $@
	chmod u+x $@

CLEANFILES = $(BUILT_SOURCES) gnucash gnucash-valgrind gnucash-gdb

# We handle gnucash scripts in a somewhat unexpected way, but we do
# this so that a user who doesn't necessarily have the right
# directories in their path can still invoke these commands via their
# full path, say /some/dir/not/in/path/gnucash and still have the
# right thing happen (i.e. they'll still get the right guile, and the
# right scripts if they sub-exec anything from their scripts).  If you
# want to add another gnucash script, please add the name here (which
# will cause the bindir wrapper to be created, and then put the actual
# code in a script of the same name in ./overrides.  Oh, and don't
# forget to add your script to configure.in's "Adjustments" section if
# you need to.
#
# For testing and other reasons, overrides/* scripts should not modify
# the path to re-insert the overrides dir.  This should only be done
# by these top-level "common" scripts.
gnc_common_scripts = gnucash-env gnucash-make-guids

bin_SCRIPTS = ${gnc_common_scripts} update-gnucash-gconf gnucash gnucash-valgrind gnucash-gdb

# if you change gncoverridedir, make sure you change ./overrides/Makefile.am too.
gncoverridesdir = ${GNC_LIBEXECDIR}/overrides

EXTRA_DIST = generate-gnc-script update-gnucash-gconf.in \
	gnucash.in gnucash-valgrind.in gnucash-gdb.in

## Gnucash scripts -- real code is in overrides, these just get you there.
${gnc_common_scripts}: generate-gnc-script ${top_builddir}/config.status
	${srcdir}/generate-gnc-script $@ "${gncoverridesdir}"

CLEANFILES += ${gnc_common_scripts} update-gnucash-gconf

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
update-gnucash-gconf: update-gnucash-gconf.in \
		${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-GCONF_SCHEMA_FILE_DIR-@#${GCONF_SCHEMA_FILE_DIR}#g'
	mv $@.tmp $@
	chmod +x $@

INCLUDES = -DG_LOG_DOMAIN=\"gnc.bin\"
