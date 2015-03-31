SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-standard-reports.la

libgncmod_standard_reports_la_SOURCES = \
  gncmod-standard-reports.c

libgncmod_standard_reports_la_LDFLAGS = -avoid-version

libgncmod_standard_reports_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/report/
gncscmmod_DATA = \
   standard-reports.scm

gncscmrptdir = ${GNC_SCM_INSTALL_DIR}/gnucash/report/standard-reports
gncscmrpt_DATA = \
   account-piecharts.scm \
   account-summary.scm \
   advanced-portfolio.scm \
   average-balance.scm \
   balance-sheet.scm \
   budget.scm \
   budget-balance-sheet.scm \
   budget-barchart.scm \
   budget-flow.scm \
   budget-income-statement.scm \
   cash-flow.scm \
   category-barchart.scm \
   daily-reports.scm \
   equity-statement.scm \
   general-journal.scm \
   general-ledger.scm \
   income-statement.scm \
   net-barchart.scm \
   net-linechart.scm \
   portfolio.scm \
   price-scatter.scm \
   register.scm \
   sx-summary.scm \
   trial-balance.scm \
   transaction.scm



if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncscmmod_DATA} ${gncscmrpt_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
	mkdir -p  gnucash/report/standard-reports
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
	( cd gnucash/report/standard-reports; for A in $(gncscmrpt_DATA) ; do $(LN_S) -f ../../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/html \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/html \
  --library-dir    ${top_builddir}/src/report/report-system

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmrptcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report/standard-reports
gncscmrptcache_DATA = $(gncscmrpt_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

EXTRA_DIST = \
	${gncscmmod_DATA} \
	${gncscmrpt_DATA}

CLEANFILES = \
	.scm-links \
	${SCM_FILE_LINK} \
	${gncscmmodcache_DATA} \
	${gncscmrptcache_DATA}

DISTCLEANFILES =

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.report.standard\"
