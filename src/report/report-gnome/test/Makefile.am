TESTS = \
  test-link-module \
  test-load-module 

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/network-utils \
  --gnc-module-dir ${top_builddir}/src/scm \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/app-file \
  --gnc-module-dir ${top_builddir}/src/gnome \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/report-gnome \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_srcdir}/lib \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/app-utils \
  --guile-load-dir ${top_srcdir}/src/report/report-system \
  --guile-load-dir ${top_srcdir}/src/gnome-utils \
  --guile-load-dir ${top_srcdir}/src/gnome \
  --library-dir    ${top_srcdir}/src/gnome-utils \
  --library-dir    ${top_srcdir}/src/gnome

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

LDADD = ../libgncmod-report-gnome.la 

noinst_PROGRAMS = test-link-module 

EXTRA_DIST = test-load-module
