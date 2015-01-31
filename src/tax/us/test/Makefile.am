TESTS = test-link-module

# Some of these dirs may not be needed anymore.
GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/tax/us \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/tax/us \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module

TESTS_ENVIRONMENT = \
  GUILE="${GUILE}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS=test-link-module
test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=../libgncmod-tax-us.la ${GUILE_LIBS} ${GLIB_LIBS}
