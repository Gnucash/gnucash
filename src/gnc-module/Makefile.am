SUBDIRS = . test

AM_CPPFLAGS = \
   -I${top_srcdir}/lib/libc \
   -I${top_srcdir}/src \
   -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/libqof/qof \
   ${GLIB_CFLAGS} \
   ${GUILE_CFLAGS}

lib_LTLIBRARIES = libgnc-module.la

libgnc_module_la_SOURCES = gnc-module.c swig-gnc-module.c

libgnc_module_la_LIBADD = \
  ../core-utils/libgnc-core-utils.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash
gncscmmod_DATA = gnc-module.scm

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-module-api.h \
  gnc-module.h

noinst_DATA = .scm-links

if BUILDING_FROM_VCS
swig-gnc-module.c: gnc-module.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  gnc-module.i

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncscmmod_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/libqof/qof

%.go : %.scm .scm-links $(lib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)
endif

clean-local:
	rm -rf gnucash

CLEANFILES = .scm-links ${SCM_FILE_LINKS} ${gncscmmodcache_DATA}
MAINTAINERCLEANFILES = swig-gnc-module.c

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.module\"
