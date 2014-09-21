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
  -I${top_builddir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/report/report-system \
  ${GUILE_CFLAGS} \
  ${GTK_MAC_CFLAGS}

SUFFIXES = .rc

config_DATA = environment
configdir = ${GNC_CONFIGDIR}

# Some settings are platform dependent. Let's define them per platform.
if PLATFORM_WIN32
# Windows specific settings go here:
GNUCASH_RESOURCE_FILE = gnucash.rc
dist_noinst_DATA = gnucash.rc

.rc.o:
	$(AM_V_GEN)$(RC) -I${top_srcdir}/src/pixmaps -i '$<' --input-format=rc -o '$@' -O coff

else !PLATFORM_WIN32
# All other platforms use these settings:
PLATFORM_FILES = gnucash-valgrind

endif !PLATFORM_WIN32

BIN_NAME = gnucash
bin_PROGRAMS = ${BIN_NAME}
gnucash_SOURCES = gnucash-bin.c ${GNUCASH_RESOURCE_FILE}
gnucash_LDADD = \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${GTK_LIBS}

if WITH_GOOGLE_PROFILER
gnucash_LDADD += -lprofiler
endif

GNUCASH_BIN_INSTALL_NAME=`echo ${BIN_NAME} | sed -e '$(transform)'`

gnucash-valgrind: gnucash-valgrind.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e "s#@-TOP_SRC_DIR-@#${abs_top_srcdir}#g" \
	    -e "s#@-GNUCASH_BIN_INSTALL_NAME-@#${GNUCASH_BIN_INSTALL_NAME}#g"
	mv $@.tmp $@
	chmod u+x $@

environment: environment.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e '/@-NOTE.*-@/ D'
if CUSTOM_GNC_DBD_DIR
	echo 'GNC_DBD_DIR=@GNC_DBD_DIR@' >> $@.tmp
endif
    # Set XDG_DATA_DIRS if necessary.  The three components of the search path are the
    # directory used by GnuCash, whatever was specified in the environment at run time, and
    # the default value specified via configure.
	if [ "a$(datadir)" != "a/usr/share" ] && [ "a$(datadir)" != "a/usr/local/share" ]; \
	then \
		echo >> $@.tmp; \
		echo "# GnuCash was not installed in the default location" >> $@.tmp; \
		echo "# XDG_DATA_DIRS will be set so that our documentation" >> $@.tmp; \
		echo "# and gsettings schemas are found." >> $@.tmp; \
		echo "XDG_DATA_DIRS=$(datadir);{XDG_DATA_DIRS};${GNC_SYSTEM_XDG_DATA_DIRS}" >> $@.tmp; \
	fi
if WITH_PYTHON
	if [ "${PYTHON_SITE_PKG}" != "${pyexecdir}" ]; \
	then \
		echo  >> $@.tmp; \
		echo "# Define PYTHONPATH for non default installation path." >> $@.tmp; \
		echo "PYTHONPATH=${pyexecdir};{PYTHONPATH}" >> $@.tmp; \
	fi
endif
	mv $@.tmp $@

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
	${PLATFORM_FILES}

# if you change gncoverridedir, make sure you change ./overrides/Makefile.am too.
gncoverridesdir = ${GNC_LIBEXECDIR}/overrides

## Gnucash scripts -- real code is in overrides, these just get you there.
${gnc_common_scripts}: generate-gnc-script ${top_builddir}/config.status
	${srcdir}/generate-gnc-script $@ "${gncoverridesdir}"

CLEANFILES += ${gnc_common_scripts}

endif

EXTRA_DIST = \
    generate-gnc-script \
	gnucash-valgrind.in \
	environment.in

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.bin\"
