
gncscmdir = ${GNC_SCM_INSTALL_DIR}/report

gncscm_DATA = \
  account-summary.scm \
  average-balance.scm \
  category-barchart.scm \
  hello-world.scm \
  income-expense-graph.scm \
  income-or-expense-pie.scm \
  pnl.scm \
  portfolio.scm \
  register.scm \
  report-list.scm \
  stylesheet-fancy.scm \
  stylesheet-plain.scm \
  table-test.scm \
  taxtxf.scm \
  transaction-report.scm \
  txf-export-help.scm \
  txf-export.scm \
  view-column.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA}

guile-strings.c: ${gncscm_DATA}
	rm -f guile-strings.c
	guile -s ../xgettext.scm ${gncscm_DATA}
CLEANFILES += guile-strings.c

all-local: guile-strings.c
