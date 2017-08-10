TESTS = test-link-module

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/tax/us \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --gnc-module-dir ${top_builddir}/gnucash/report/locale-specific/us \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/tax/us \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/app-utils

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = test-link-module

testit:
	${TESTS_ENVIRONMENT} gdb guile

test_link_module_SOURCES = test-link-module.c
test_link_module_LDADD= \
  ${top_builddir}/gnucash/report/locale-specific/us/libgncmod-locale-reports-us.la

EXTRA_DIST = CMakeLists.txt
