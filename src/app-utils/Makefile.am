SUBDIRS = . test
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-app-utils.la libgw-app-utils.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLIB_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${GTK_CFLAGS}

libgncmod_app_utils_la_SOURCES = \
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
  gnc-helpers.h \
  gnc-ui-common.h \
  gnc-ui-util.h \
  guile-util.h \
  option-util.h

libgncmod_app_utils_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/engine/libgw-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${GCONF_LIBS} \
  ${GTK_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

libgw_app_utils_la_LIBADD = \
  libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/engine/libgw-engine.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

nodist_libgw_app_utils_la_SOURCES = gw-app-utils.c

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = app-utils.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  c-interface.scm \
  config-var.scm \
  date-utilities.scm \
  hooks.scm \
  kvp-option-registry.scm \
  options.scm \
  prefs.scm \
  simple-obj.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-app-utils-spec.scm
nodist_gwmod_DATA = gw-app-utils.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncmod_DATA} \
  ${gncscm_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-app-utils-spec.scm
#For executing test cases
SCM_FILE_LINKS += \
  ${gncmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash g-wrapped
	mkdir -p  gnucash g-wrapped
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
	touch .scm-links

clean-local:
	$(RM) -rf gnucash g-wrapped

.INTERMEDIATE: gwrap-files

gw-app-utils.scm gw-app-utils.c gw-app-utils.h: \
  gwrap-files

gwrap-files: \
  .scm-links gw-app-utils-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (primitive-load \"./gw-app-utils-spec.scm\") \
	  (gw:generate-wrapset \"gw-app-utils\")"
	touch $@
	( cd g-wrapped; $(LN_S) ../gw-*.scm . )

BUILT_SOURCES = gw-app-utils.scm gw-app-utils.c gw-app-utils.h
CLEANFILES = $(BUILT_SOURCES) ${SCM_FILE_LINKS}  \
	.scm-links gw-app-utils.html
