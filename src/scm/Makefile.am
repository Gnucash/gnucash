
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

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module

%.go : %.scm .scm-links
	$(shell ${top_builddir}/src/gnc-test-env --no-exports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${GNC_SCM_INSTALL_DIR}/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${GNC_SCM_INSTALL_DIR}/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash

SCM_FILES = ${gncscm_DATA} ${gncscmmod_DATA}

EXTRA_DIST = \
  build-config.scm.in \
  config \
  ${SCM_FILES}

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
build-config.scm: ${srcdir}/build-config.scm.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's#@-VERSION-@#${VERSION}#' \
            -e 's#@-GNC_HELPDIR-@#${GNC_HELPDIR}#'
	mv $@.tmp $@

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = build-config.scm
