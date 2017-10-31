TESTS = ${SCM_TESTS}

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/html \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --gnc-module-dir ${top_builddir}/gnucash/report/stylesheets \
\
  --guile-load-dir ${top_builddir}/common/test-core \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
  --guile-load-dir ${top_builddir}/gnucash/report/report-system \
  --guile-load-dir ${top_builddir}/gnucash/report/stylesheets \
\
  --library-dir    ${top_builddir}/common/test-core \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/gnucash/html \
  --library-dir    ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/gnucash/report/report-gnome

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

SCM_TESTS = test-load-report-stylesheets-module
SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@

EXTRA_DIST = \
  ${SCM_TEST_SRCS} \
  CMakeLists.txt
