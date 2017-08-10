AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/common \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

TESTS = \
  test-link-module \
  test-load-module \
  $(SCM_TESTS)

SCM_TESTS = \
	test-collectors \
	test-list-extras \
	test-report-utilities

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/html \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system/test \
  --gnc-module-dir ${top_builddir}/libgnucash/engine/test \
  --gnc-module-dir ${top_builddir}/libgnucash/backend/xml \
  --gnc-module-dir ${top_builddir}/libgnucash/backend/sql \
  --gnc-module-dir ${top_builddir}/libgnucash/backend/dbi \
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
\
  --library-dir    ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql  \
  --library-dir    ${top_builddir}/libgnucash/backend/dbi  \
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


check_PROGRAMS = test-link-module

SCM_TEST_HELPERS = test-extras.scm

EXTRA_DIST = \
	test-load-module \
	$(SCM_TEST_HELPERS) \
	$(SCM_TEST_SRCS) \
	CMakeLists.txt

testit:
	$(TESTS_ENVIRONMENT) libtool --mode execute gdb test-link-module


.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash/report/report-system/test
	( cd gnucash/report/report-system/test; for A in $(SCM_TEST_HELPERS) ; do $(LN_S) -f $(abs_srcdir)/$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf gnucash
	$(RM) *.log

noinst_DATA = .scm-links
CLEANFILES = .scm-links
DISTCLEANFILES = $(SCM_TESTS)
