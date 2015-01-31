MODULE_TESTS=test-load-module

TESTS = $(SCM_TESTS) $(MODULE_TESTS)

SCM_TESTS = \
	test-standard-category-report \
	test-standard-net-barchart \
	test-standard-net-linechart

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/html \
  --gnc-module-dir ${top_builddir}/src/report/app-utils \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/report-system/test \
  --gnc-module-dir ${top_builddir}/src/report/standard-reports \
  --gnc-module-dir ${top_builddir}/src/report/standard-reports/test \
\
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/report/report-system/test \
  --guile-load-dir ${top_builddir}/src/report/standard-reports \
  --guile-load-dir ${top_builddir}/src/report/standard-reports/test \
\
  --library-dir    ${top_builddir}/src/report/report-system \
  --library-dir    ${top_builddir}/src/report/standard-reports \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/gnc-module

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile .scm-links
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' > $@
	chmod a+x $@

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})


SCM_TEST_HELPERS = \
	test-generic-category-report.scm \
	test-generic-net-barchart.scm \
	test-generic-net-linechart.scm

EXTRA_DIST = \
	test-load-module \
	$(SCM_TEST_HELPERS) \
	$(SCM_TEST_SRCS)

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

