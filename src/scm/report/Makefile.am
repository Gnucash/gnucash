
gncscmdir = ${GNC_SCM_INSTALL_DIR}/report

gncscm_DATA = \
  report-list.scm \
  stylesheet-fancy.scm \
  stylesheet-plain.scm \
  txf-export-help.scm \
  txf-export.scm \
  view-column.scm \
  welcome-to-gnucash.scm

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
  pnl.scm \
  portfolio.scm \
  price-scatter.scm \
  register.scm \
  taxtxf.scm \
  transaction.scm

SCM_FILES = ${gncscm_DATA} ${gncscmmod_DATA}

EXTRA_DIST = \
  .cvsignore \
  ${SCM_FILES}

guile-strings.c: ${SCM_FILES}
	rm -f guile-strings.c
	guile -s ../xgettext.scm ${SCM_FILES}
CLEANFILES += guile-strings.c

all-local: guile-strings.c
