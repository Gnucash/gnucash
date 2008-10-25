SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-app-utils.la

AM_CPPFLAGS = \
  -I$(top_srcdir)/lib/libc \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${GTK_CFLAGS}

libgncmod_app_utils_la_SOURCES = \
  swig-app-utils.c \
  file-utils.c \
  gfec.c \
  gnc-account-merge.c \
  gnc-accounting-period.c \
  gnc-component-manager.c \
  gnc-druid.c \
  gnc-druid-cb.c \
  gnc-druid-provider.c \
  gnc-druid-provider-desc.c \
  gnc-druid-provider-desc-edge.c \
  gnc-druid-provider-desc-file.c \
  gnc-druid-provider-desc-multifile.c \
  gnc-druid-provider-file-cb.c \
  gnc-euro.c \
  gnc-exp-parser.c \
  gnc-gettext-util.c \
  gnc-helpers.c \
  gnc-sx-instance-model.c \
  gncmod-app-utils.c \
  gnc-ui-util.c \
  guile-util.c \
  option-util.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  file-utils.h \
  gfec.h \
  gnc-basic-gobject.h \
  gnc-account-merge.h \
  gnc-accounting-period.h \
  gnc-component-manager.h \
  gnc-druid.h \
  gnc-druid-cb.h \
  gnc-druid-provider.h \
  gnc-druid-provider-desc.h \
  gnc-druid-provider-desc-edge.h \
  gnc-druid-provider-desc-file.h \
  gnc-druid-provider-desc-multifile.h \
  gnc-druid-provider-file-cb.h \
  gnc-euro.h \
  gnc-exp-parser.h \
  gnc-gettext-util.h \
  gnc-help-utils.h \
  gnc-helpers.h \
  gnc-sx-instance-model.h \
  gnc-ui-common.h \
  gnc-ui-util.h \
  guile-util.h \
  option-util.h

libgncmod_app_utils_la_LDFLAGS = -avoid-version

libgncmod_app_utils_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${GCONF_LIBS} \
  ${GTK_LIBS} \
  ${GUILE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

if BUILDING_FROM_SVN
swig-app-utils.c: app-utils.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = app-utils.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  c-interface.scm \
  config-var.scm \
  date-utilities.scm \
  hooks.scm \
  options.scm \
  prefs.scm \
  simple-obj.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  app-utils.i \
  ${gncmod_DATA} \
  ${gncscm_DATA}

if OS_WIN32
libgncmod_app_utils_la_SOURCES += gnc-help-utils.c
libgncmod_app_utils_la_LIBADD += $(HTMLHELP_LIBS)
else
EXTRA_DIST += gnc-help-utils.c
endif

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
	touch .scm-links

CLEANFILES = ${SCM_FILE_LINKS} gnucash .scm-links
MAINTAINERCLEANFILES = swig-app-utils.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.app-utils\"
