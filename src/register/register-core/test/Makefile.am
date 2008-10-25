TESTS =  \
  test-link-module
#  test-load-module

#GNC_TEST_DEPS := \
#

#TESTS_ENVIRONMENT := \
#  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD= \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ../libgncmod-register-core.la

#EXTRA_DIST = test-load-module

AM_CPPFLAGS = ${GLIB_CFLAGS} -I${top_srcdir}/src/test-core -I..
