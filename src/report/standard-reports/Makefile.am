SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-standard-reports.la

libgncmod_standard_reports_la_SOURCES = \
  gncmod-standard-reports.c 

libgncmod_standard_reports_la_LDFLAGS = -module

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} ${GLIB_CFLAGS}

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf ${srcdir} report 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
   account-piecharts.scm \
   account-summary.scm \
   advanced-portfolio.scm \
   average-balance.scm \
   balance-sheet.scm \
   cash-flow.scm \
   category-barchart.scm \
   daily-reports.scm \
   equity-statement.scm \
   net-barchart.scm \
   income-statement.scm \
   portfolio.scm \
   price-scatter.scm \
   register.scm \
   standard-reports.scm \
   trial-balance.scm \
   general-ledger.scm \
   transaction.scm 

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links
