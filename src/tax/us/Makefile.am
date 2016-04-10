SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-tax-us.la

libgncmod_tax_us_la_SOURCES = \
  gncmod-tax-us.c 

libgncmod_tax_us_la_LDFLAGS = -avoid-version

libgncmod_tax_us_la_LIBADD = \
 ${GUILE_LIBS} \
 ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} ${GUILE_CFLAGS}

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/tax
gncscmmod_DATA = us.scm de_DE.scm

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscm_DATA = txf.scm txf-help.scm txf-de_DE.scm txf-help-de_DE.scm 

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p gnucash
	mkdir -p gnucash/tax
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/tax; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/tax
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}

CLEANFILES = .scm-links ${gncscmmodcache_DATA} ${gncscmcache_DATA}

DISTCLEANFILES = ${SCM_FILE_LINKS}
