TESTS=test-load-module test-link-module 

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/app-file \
  --gnc-module-dir ${top_builddir}/src/tax/us \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/locale-specific/us \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_srcdir}/lib \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/gnome \
  --library-dir    ${top_srcdir}/src/gnome

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

EXTRA_DIST = test-load-module

noinst_PROGRAMS=test-link-module

testit:
	${TESTS_ENVIRONMENT} gdb guile

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD= \
 ${top_srcdir}/src/report/locale-specific/us/libgncmod-locale-reports-us.la


 