TESTS = $(SCM_TESTS)

SCM_TESTS = \
	test-cash-flow \
	test-cashflow-barchart \
	test-standard-category-report \
	test-standard-net-barchart \
	test-standard-net-linechart

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/engine/test \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/html \
  --gnc-module-dir ${top_builddir}/gnucash/report/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system/test \
\
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/engine/test \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
  --guile-load-dir ${top_builddir}/gnucash/report/report-system \
  --guile-load-dir ${top_builddir}/gnucash/report/report-system/test \
  --guile-load-dir ${top_builddir}/gnucash/report/standard-reports \
  --guile-load-dir ${top_builddir}/gnucash/report/standard-reports/test \
  --guile-load-dir ${top_builddir}/gnucash/report/stylesheets \
\
  --library-dir    ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/libgnucash/gnc-module

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile .scm-links
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})


SCM_TEST_HELPERS = \
	test-generic-category-report.scm \
	test-generic-net-barchart.scm \
	test-generic-net-linechart.scm

EXTRA_DIST = \
	$(SCM_TEST_HELPERS) \
	$(SCM_TEST_SRCS) \
	CMakeLists.txt

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash/report/standard-reports/test
	( cd gnucash/report/standard-reports/test; for A in $(SCM_TEST_HELPERS) ; do $(LN_S) -f $(abs_srcdir)/$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

interp:
	$(TESTS_ENVIRONMENT) ${GUILE} --debug

debug:
	$(TESTS_ENVIRONMENT) gdb --args $(shell cat $(TEST))

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links
CLEANFILES = .scm-links *.log
DISTCLEANFILES = $(SCM_TESTS)

