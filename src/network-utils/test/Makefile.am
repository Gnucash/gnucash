TESTS =  \
  test-link-module \
  test-load-module

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/network-utils \
  --guile-load-dir ${G_WRAP_MODULE_DIR}

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=../libgncmod-network-utils.la

EXTRA_DIST = test-load-module

AM_CFLAGS = \
  -I${top_srcdir}/src/test-core \
  -I.. \
  ${GLIB_CFLAGS}
