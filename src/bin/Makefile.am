# Order is important here.
if !PLATFORM_WIN32
SUBDIRS = . overrides test
else
SUBDIRS = . test
endif

AM_CPPFLAGS = -I${top_builddir} ${GLIB_CFLAGS} ${GNOME_CFLAGS} ${GTK_CFLAGS} \
  -DPKGSYSCONFDIR=\"${GNC_CONFIGDIR}\" \
  -DPKGDATADIR=\"${GNC_SHAREDIR}\" \
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
  ${GUILE_INCS} \
  ${IGE_MAC_CFLAGS}

SUFFIXES = .rc .res

# Some settings are platform dependent. Let's define them per platform.
if PLATFORM_WIN32
# Windows specific settings go here:
GNUCASH_RESOURCE_FILE = gnucash.rc
dist_noinst_DATA = gnucash.rc

.rc.res:
	$(AM_V_GEN)$(RC) -i '$<' --input-format=rc -o '$@' -O coff
	
configdir = ${GNC_CONFIGDIR}
config_DATA = environment

else !PLATFORM_WIN32
if PLATFORM_OSX_QUARTZ
# OSX/Quartz specific settings go here:
PLATFORM_FILES = gnucash-setup-env \
	gnucash-launcher \
	gnucash-gdb \
	gnucash-valgrind

if CUSTOM_GNC_DBD_DIR
gnc_dbd_dir_override = export GNC_DBD_DIR="@GNC_DBD_DIR@"
endif

else !PLATFORM_OSX_QUARTZ
# All other platforms use these settings:
PLATFORM_FILES = gnucash-valgrind
configdir = ${GNC_CONFIGDIR}
config_DATA = environment

endif !PLATFORM_OSX_QUARTZ
endif !PLATFORM_WIN32

BIN_NAME = gnucash
bin_PROGRAMS = ${BIN_NAME}
gnucash_SOURCES = gnucash-bin.c ${GNUCASH_RESOURCE_FILE}
gnucash_LDADD = \
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

GNUCASH_BIN_INSTALL_NAME=`echo ${BIN_NAME} | sed -e '$(transform)'`

gnucash-launcher: gnucash-launcher.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e "s#@-GNUCASH_BIN_INSTALL_NAME-@#${GNUCASH_BIN_INSTALL_NAME}#g"
	mv $@.tmp $@
	chmod u+x $@

gnucash-gdb: gnucash-gdb.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e "s#@-GNUCASH_BIN_INSTALL_NAME-@#${GNUCASH_BIN_INSTALL_NAME}#g"
	mv $@.tmp $@
	chmod u+x $@

gnucash-valgrind: gnucash-valgrind.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e "s#@-TOP_SRC_DIR-@#${abs_top_srcdir}#g" \
	    -e "s#@-GNUCASH_BIN_INSTALL_NAME-@#${GNUCASH_BIN_INSTALL_NAME}#g"
	mv $@.tmp $@
	chmod u+x $@

gnucash-setup-env: gnucash-setup-env-osx.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-BIN_DIR-@#${bindir}#g' \
	    -e 's#@-GNC_DBD_DIR-@#${GNC_DBD_DIR}#g' \
	    -e 's#@-GNC_STANDARD_REPORTS_DIR-@#${GNC_SHAREDIR}/guile-modules/gnucash/report/standard-reports#g' \
	    -e 's#@-GNC_GUILE_MODULE_DIR-@#${GNC_SHAREDIR}/guile-modules#g' \
	    -e 's#@-GNC_SCM_INSTALL_DIR-@#${GNC_SCM_INSTALL_DIR}#g' \
	    -e 's#@-GNC_LIB_INSTALLDIR-@#${libdir}#' \
	    -e 's#@-GNC_PKGLIB_INSTALLDIR-@#${pkglibdir}#g' \
	    -e "s#@-TOP_SRC_DIR-@#`pwd`/${top_srcdir}#g" \
	    -e 's#@-PATH_SEPARATOR-@#${PATH_SEPARATOR}#g' \
	    -e 's#@-PREFIX-@#${prefix}#g'
	mv $@.tmp $@
	chmod u+x $@

environment: ${GNUCASH_ENVIRONMENT}.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-BIN_DIR-@#${bindir}#g' \
	    -e 's#@-GNC_STANDARD_REPORTS_DIR-@#${GNC_SHAREDIR}/guile-modules/gnucash/report/standard-reports#g' \
	    -e 's#@-GNC_GUILE_MODULE_DIR-@#${GNC_SHAREDIR}/guile-modules#g' \
	    -e 's#@-GNC_SCM_INSTALL_DIR-@#${GNC_SCM_INSTALL_DIR}#g' \
	    -e 's#@-GNC_LIB_INSTALLDIR-@#${libdir}#' \
	    -e 's#@-GNC_PKGLIB_INSTALLDIR-@#${pkglibdir}#g' \
	    -e "s#@-TOP_SRC_DIR-@#`pwd`/${top_srcdir}#g" \
	    -e 's#@-PREFIX-@#${prefix}#g'
if CUSTOM_GNC_DBD_DIR
	echo 'GNC_DBD_DIR=@GNC_DBD_DIR@' >> $@.tmp
endif
	mv $@.tmp $@
	chmod u+x $@

CLEANFILES = $(BUILT_SOURCES) ${config_DATA} ${PLATFORM_FILES}

if !PLATFORM_WIN32
# The gnucash scripts don't make sense on Windows, so will only be
# generated and included on the other platforms.
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

bin_SCRIPTS = \
    ${gnc_common_scripts} \
	update-gnucash-gconf \
	${PLATFORM_FILES}

# if you change gncoverridedir, make sure you change ./overrides/Makefile.am too.
gncoverridesdir = ${GNC_LIBEXECDIR}/overrides

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

endif

EXTRA_DIST = \
    generate-gnc-script \
	update-gnucash-gconf.in \
	gnucash-launcher.in \
	gnucash-valgrind.in \
	gnucash-gdb.in \
	environment.in \
	environment-win32.in

INCLUDES = -DG_LOG_DOMAIN=\"gnc.bin\"
