AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

TESTS = \
  test-link-module \
  test-load-module \
  $(SCM_TESTS)

SCM_TESTS = \
	test-collectors \
	test-list-extras \
	test-test-extras

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/html \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/report-system/test \
\
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/report/report-system/test \
\
  --library-dir    ${top_builddir}/src/report/report-system \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql  \
  --library-dir    ${top_builddir}/src/gnc-module

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile .scm-links
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' > $@
	chmod a+x $@

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})


check_PROGRAMS = test-link-module

SCM_TEST_HELPERS = test-extras.scm

EXTRA_DIST = \
	test-load-module \
	$(SCM_TEST_HELPERS) \
	$(SCM_TEST_SRCS)

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

noinst_DATA = .scm-links
CLEANFILES = .scm-links
DISTCLEANFILES = $(SCM_TESTS)
