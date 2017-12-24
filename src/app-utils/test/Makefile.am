TESTS = \
  test-link-module \
  test-load-module \
  test-exp-parser \
  test-scm-query-string \
  test-print-parse-amount \
  test-sx \
  $(SCM_TESTS)

SCM_TESTS =   test-date-utilities

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

test_exp_parser_SOURCES = \
  test-exp-parser.c

test_print_parse_amount_SOURCES = \
  test-print-parse-amount.c

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/engine/test \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/app-utils

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile .scm-links
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  SRCDIR=${srcdir} \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

LDADD = \
   ${top_builddir}/src/libqof/qof/libgnc-qof.la \
   ${top_builddir}/src/core-utils/libgnc-core-utils.la \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   ${top_builddir}/src/gnc-module/libgnc-module.la \
   ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
   ${top_builddir}/src/test-core/libtest-core.la \
   ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
   ${GUILE_LIBS}

check_PROGRAMS = \
  test-link-module \
  test-exp-parser \
  test-print-parse-amount \
  test-scm-query-string \
  test-print-queries \
  test-sx

EXTRA_DIST = \
  test-load-module \
  CMakeLists.txt

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash/app-utils/test
	( cd gnucash/app-utils/test; for A in $(SCM_TEST_HELPERS) ; do $(LN_S) -f $(abs_srcdir)/$$A . ; done )
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
