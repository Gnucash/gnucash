
gncscmdir = ${GNC_SCM_INSTALL_DIR}/report

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report

gncscmmod_DATA = \
  account-piecharts.scm \
  account-summary.scm \
  average-balance.scm \
  balance-sheet.scm \
  category-barchart.scm \
  hello-world.scm \
  iframe-url.scm \
  net-barchart.scm \
  portfolio.scm \
  pnl.scm \
  price-scatter.scm \
  register.scm \
  taxtxf.scm \
  transaction.scm

gncscm_DATA = \
  report-list.scm \
  stylesheet-fancy.scm \
  stylesheet-plain.scm \
  txf-export-help.scm \
  txf-export.scm \
  view-column.scm \
  welcome-to-gnucash.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA}

guile-strings.c: ${gncscm_DATA}
	rm -f guile-strings.c
	guile -s ../xgettext.scm ${gncscm_DATA}
CLEANFILES += guile-strings.c

all-local: guile-strings.c
