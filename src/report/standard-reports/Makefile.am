SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-standard-reports.la

libgncmod_standard_reports_la_SOURCES = \
  gncmod-standard-reports.c 

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} ${GLIB_CFLAGS}

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
   account-piecharts.scm \
   account-summary.scm \
   advanced-portfolio.scm \
   average-balance.scm \
   balance-sheet.scm \
   cash-flow.scm \
   budget.scm \
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
   general-journal.scm \
   general-ledger.scm \
   transaction.scm 

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncscmmod_DATA}
endif

.scm-links:
	rm -f gnucash report
	$(LN_S) -f . gnucash
	$(LN_S) -f ${srcdir} report 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
