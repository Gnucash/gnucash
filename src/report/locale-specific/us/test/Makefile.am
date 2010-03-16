TESTS = test-link-module

GNC_TEST_DEPS = @GNC_TEST_SRFI_LOAD_CMD@ \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/tax/us \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/locale-specific/us \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/tax/us \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/app-utils

TESTS_ENVIRONMENT = \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = test-link-module

testit:
	${TESTS_ENVIRONMENT} gdb guile

test_link_module_SOURCES = test-link-module.c
test_link_module_LDADD= \
  ${top_builddir}/src/report/locale-specific/us/libgncmod-locale-reports-us.la
