# 2003-03-11 TomF changes for Gnome-2 branch, 6th batch
# src/network-utils/test/Makefile.am:
# Add libgncmod-engine.la to fix undefined ref to `gnc_should_log'

TESTS =  \
  test-link-module \
  test-load-module

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/network-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD= \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ../libgncmod-network-utils.la

EXTRA_DIST = test-load-module

AM_CFLAGS = \
  -I${top_srcdir}/src/test-core \
  -I.. \
  ${GLIB_CFLAGS}
