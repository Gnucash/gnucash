
SUBDIRS = gnumeric printing

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash

gncscmmod_DATA = process.scm main.scm price-quotes.scm

gnc_regular_scm_files = \
  build-config.scm \
  command-line.scm \
  doc.scm \
  help-topics-index.scm \
  main-window.scm \
  path.scm \
  substring-search.scm \
  tip-list.scm \
  tip-of-the-day.scm \
  xml-generator.scm

gncscm_DATA = ${gnc_regular_scm_files}

noinst_DATA = .scm-links

configdir = ${GNC_CONFIGDIR}
config_DATA = config

.scm-links: 
	rm -f gnucash
	ln -sf . gnucash
	touch .scm-links

CLEANFILES = .scm-links gnucash 

SCM_FILES = ${gncscm_DATA} ${gncscmmod_DATA}

EXTRA_DIST = \
  .cvsignore \
  build-config.scm.in \
  config \
  startup-design.txt \
  ${SCM_FILES}

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
build-config.scm: build-config.scm.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:' \
            -e 's:@-GNC_CONFIGDIR-@:${GNC_CONFIGDIR}:' \
            -e 's:@-GNC_SHAREDIR-@:${GNC_SHAREDIR}:' \
            -e 's:@-GNC_HELPDIR-@:${GNC_HELPDIR}:'
	mv $@.tmp $@
CLEANFILES += build-config.scm
