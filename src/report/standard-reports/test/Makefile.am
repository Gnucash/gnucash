MODULE_TESTS=test-load-module

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
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/gnc-module


TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GNC_BUILDDIR=`\cd ${top_builddir} && pwd` \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

EXTRA_DIST = test-load-module

SCM_TESTS = \
	test-standard-category-report.scm \
	test-standard-net-barchart.scm \
	test-standard-net-linechart.scm

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
	test-generic-category-report.scm \
	test-generic-net-barchart.scm \
	test-generic-net-linechart.scm \
	$(SCM_TESTS)
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash/report/standard-reports/test
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report/standard-reports/test; for A in $(SCM_FILE_LINKS) ; do $(LN_S) -f ../../../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

$(patsubst %.scm,%,$(SCM_TESTS)): %: Makefile .scm-links
	echo 'guile --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' > $@
	chmod a+x $@

interp:
	$(TESTS_ENVIRONMENT) guile --debug

debug:
	$(TESTS_ENVIRONMENT) gdb --args $(shell cat $(TEST))

TESTS = $(patsubst %.scm,%,$(SCM_TESTS)) $(MODULE_TESTS)

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links
CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}


