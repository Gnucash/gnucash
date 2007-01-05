TESTS = test-link-module test-load-module 

# Some of these dirs may not be needed anymore.
GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/core-utils \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/utility-reports \
  --gnc-module-dir ${top_builddir}/src/report/stylesheets \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/tax/us \
  --gnc-module-dir ${top_srcdir}/src/tax/us \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/lib \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnome \
  --library-dir ${top_builddir}/src/gnome-utils \
  --library-dir ${top_builddir}/src/gnome

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

EXTRA_DIST = test-load-module

check_PROGRAMS=test-link-module 
test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=../libgncmod-tax-us.la ${GUILE_LIBS} ${GLIB_LIBS}
