
SUBDIRS = gnumeric printing qif-import report srfi

gncscmdir = ${GNC_SCM_INSTALL_DIR}

gnc_autogen_scm_files = \
  bootstrap.scm

gnc_regular_scm_files = \
  c-interface.scm \
  command-line.scm \
  config-var.scm \
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
  tip-of-the-day.scm \
  tip-list.scm \
  utilities.scm \
  xml-generator.scm \
  iso-4217-currencies.scm \
  engine-init.scm \
  commodity-table.scm \
  commodity-import.scm \
  help-topics-index.scm

gncscm_DATA = ${gnc_autogen_scm_files} ${gnc_regular_scm_files}

EXTRA_DIST = \
  .cvsignore \
  bootstrap.scm.in \
  design.txt \
  startup-design.txt \
  ${gnc_regular_scm_files}

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
bootstrap.scm: bootstrap.scm.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-VERSION-@:${VERSION}:' \
            -e 's:@-GNC_CONFIGDIR-@:${GNC_CONFIGDIR}:' \
            -e 's:@-GNC_SHAREDIR-@:${GNC_SHAREDIR}:' \
            -e 's:@-GNC_HELPDIR-@:${GNC_HELPDIR}:'
	mv $@.tmp $@
CLEANFILES += bootstrap.scm
