gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/report
gncscmmod_DATA = \
   aging.scm \
   customer-summary.scm \
   easy-invoice.scm \
   fancy-invoice.scm \
   taxinvoice.scm \
   invoice.scm \
   job-report.scm \
   owner-report.scm \
   payables.scm \
   receivables.scm \
   balsheet-eg.scm \
   business-reports.scm

# Note: list these files separately because they can't be compiled by guile2
#       templates are a mixture of scheme and hmtl
#       css files are not scheme at all
gncscmtpldir = ${GNC_SCM_INSTALL_DIR}/gnucash/report
gncscmtpl_DATA = \
   taxinvoice.eguile.scm \
   taxinvoice.css \
   balsheet-eg.css \
   balsheet-eg.eguile.scm

if GNUCASH_SEPARATE_BUILDDIR
SCM_FILE_LINKS = ${gncscmmod_DATA} \
  ${gncscmtpl_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
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
  --gnc-module-dir ${top_builddir}/src/report/standard-reports \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/report/standard-reports \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${top_builddir}/src/gnc-test-env --no-exports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${GNC_SCM_INSTALL_DIR}/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscmtpl_DATA}

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}

AM_CPPFLAGS = -DG_LOG_DOMAIN=\"gnc.business.reports\"
