
SUBDIRS = gnumeric

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash

gncscmmod_DATA = main.scm price-quotes.scm printf.scm

gnc_regular_scm_files = \
  string.scm \
  fin.scm \
  substring-search.scm \
  xml-generator.scm

gncscm_DATA = \
  build-config.scm \
  ${gnc_regular_scm_files}

noinst_DATA = .scm-links

configdir = ${GNC_CONFIGDIR}
config_DATA = config

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gnc_regular_scm_files}
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

GUILE_COMPILE_ENV = \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine

%.go : %.scm .scm-links
	$(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)

clean-local:
	$(RM) -rf gnucash

SCM_FILES = ${gncscm_DATA} ${gncscmmod_DATA}

EXTRA_DIST = \
  build-config.scm.in \
  config \
  ${SCM_FILES} \
  CMakeLists.txt

build-config.scm: ${srcdir}/build-config.scm.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's#[@]VERSION[@]#${VERSION}#' \
            -e 's#[@]GNC_HELPDIR[@]#${GNC_HELPDIR}#'
	mv $@.tmp $@

CLEANFILES = .scm-links ${gncscmmodcache_DATA} ${gncscmcache_DATA}

DISTCLEANFILES = ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = build-config.scm
