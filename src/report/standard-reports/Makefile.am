SUBDIRS=. test

pkglib_LTLIBRARIES=libgncmod-standard-reports.la

libgncmod_standard_reports_la_SOURCES=\
  gncmod-standard-reports.c 

libgncmod_standard_reports_la_LDFLAGS=-module

INCLUDES=-I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf . report 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir=${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
   account-piecharts.scm \
   account-summary.scm \
   average-balance.scm \
   balance-sheet.scm \
   category-barchart.scm \
   net-barchart.scm \
   payables.scm \
   pnl.scm \
   portfolio.scm \
   price-scatter.scm \
   register.scm \
   standard-reports.scm \
   transaction.scm 

CLEANFILES += gnucash report .scm-links
