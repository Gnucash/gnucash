
gncscmdir = ${GNC_SCM_INSTALL_DIR}/report

gncscm_DATA = \
  account-summary.scm \
  average-balance.scm \
  balance-and-pnl.scm \
  budget-report.scm \
  folio.scm \
  hello-world.scm \
  report-list.scm \
  transaction-report.scm \
  taxtxf.scm \
  txf-export-help.scm \
  txf-export.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA}

guile-strings.c: ${gncscm_DATA}
	rm -f guile-strings.c
	guile -s ../xgettext.scm ${gncscm_DATA}
CLEANFILES += guile-strings.c

all-local: guile-strings.c
