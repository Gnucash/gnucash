SUBDIRS = . test

AM_CFLAGS = \
   -I${top_srcdir}/lib/libc \
   -I${top_srcdir}/src \
   -I${top_srcdir}/src/core-utils \
   ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS} ${GUILE_INCS}

lib_LTLIBRARIES = libgncmodule.la 
pkglib_LTLIBRARIES = libgw-gnc-module.la

libgncmodule_la_SOURCES = gnc-module.c

libgncmodule_la_LIBADD = \
  ../core-utils/libcore-utils.la \
  ${G_WRAP_LINK_ARGS} \
 ${top_builddir}/lib/libc/libc-missing.la \
  ${GLIB_LIBS} \
  -lltdl

nodist_libgw_gnc_module_la_SOURCES = gw-gnc-module.c

libgw_gnc_module_la_LIBADD = libgncmodule.la \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnc-module.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnc-module-spec.scm 
nodist_gwmod_DATA = gw-gnc-module.scm

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-module-api.h \
  gnc-module.h

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncmod_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-gnc-module-spec.scm
#For executing test cases
SCM_FILE_LINKS += ${gncmod_DATA}
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

gw-gnc-module.scm gw-gnc-module.c gw-gnc-module.h: \
  gwrap-files

gwrap-files: \
  .scm-links gw-gnc-module-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"./gw-gnc-module-spec.scm\") \
	  (gw:generate-wrapset \"gw-gnc-module\")"
	touch $@
	( cd g-wrapped; $(LN_S) -f ../gw-*.scm . )

BUILT_SOURCES = gw-gnc-module.scm gw-gnc-module.h gw-gnc-module.c
CLEANFILES = $(BUILT_SOURCES) .scm-links \
	${SCM_FILE_LINKS} gw-gnc-module.html
