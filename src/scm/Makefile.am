
SUBDIRS = gnumeric printing report

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash

gncscmmod_DATA = process.scm

gnc_autogen_scm_files = \
  bootstrap.scm

gnc_regular_scm_files = \
  c-interface.scm \
  command-line.scm \
  commodity-import.scm \
  commodity-utilities.scm \
  config-var.scm \
  date-utilities.scm \
  depend.scm \
  doc.scm \
  extensions.scm \
  graph.scm \
  help-topics-index.scm \
  hooks.scm \
  html-barchart.scm \
  html-document.scm \
  html-piechart.scm \
  html-scatter.scm \
  html-style-info.scm \
  html-style-sheet.scm \
  html-text.scm \
  html-table.scm \
  html-utilities.scm \
  main.scm \
  main-window.scm \
  options.scm \
  options-utilities.scm \
  path.scm \
  prefs.scm \
  price-quotes.scm \
  report.scm \
  report-html.scm \
  report-utilities.scm \
  slib-backup.scm \
  startup.scm \
  structure.scm \
  substring-search.scm \
  testbed.scm \
  text-export.scm \
  tip-list.scm \
  tip-of-the-day.scm \
  utilities.scm \
  xgettext.scm \
  xml-generator.scm

gncscm_DATA = ${gnc_autogen_scm_files} ${gnc_regular_scm_files}

SCM_FILES = ${gncscm_DATA} ${gncscmmod_DATA}

EXTRA_DIST = \
  .cvsignore \
  bootstrap.scm.in \
  design.txt \
  startup-design.txt \
  ${SCM_FILES}

guile-strings.c: ${SCM_FILES}
	rm -f guile-strings.c
	guile -s ./xgettext.scm ${SCM_FILES}
CLEANFILES += guile-strings.c

all-local: guile-strings.c

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
            -e 's:@-GNC_LIB_INSTALLDIR-@:${libdir}:' \
            -e 's:@-GNC_HELPDIR-@:${GNC_HELPDIR}:'
	mv $@.tmp $@
CLEANFILES += bootstrap.scm
