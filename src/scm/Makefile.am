
SUBDIRS = gnumeric printing qif-import report srfi

gncscmdir = ${GNC_SCM_INSTALL_DIR}

gncscm_DATA = \
  bootstrap.scm \
  bs-interp.scm \
  c-interface.scm \
  command-line.scm \
  config-var.scm \
  currencies.scm \
  date-utilities.scm \
  depend.scm \
  doc.scm \
  engine-interface.scm \
  engine-utilities.scm \
  extensions.scm \
  graph.scm \
  hooks.scm \
  html-generator.scm \
  main.scm \
  options.scm \
  path.scm \
  prefs.scm \
  report-utilities.scm \
  report.scm \
  slib-backup.scm \
  startup.scm \
  structure.scm \
  substring-search.scm \
  testbed.scm \
  text-export.scm \
  utilities.scm \
  xml-generator.scm


## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
bootstrap.scm: bootstrap.scm.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:' \
            -e 's:@-GNC_CONFIGDIR-@:${GNC_CONFIGDIR}:' \
            -e 's:@-GNC_SHAREDIR-@:${GNC_SHAREDIR}:'
	mv $@.tmp $@

CLEANFILES += bootstrap.scm
