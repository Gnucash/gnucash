TESTS=test-load-module 

GNC_TEST_DEPS := @GNC_TEST_SRFI_LOAD_CMD@ \
  --gnc-module-dir ${top_builddir}/src/core-utils \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/scm \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/utility-reports \
  --gnc-module-dir ${top_builddir}/src/report/stylesheets \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/network-utils \
  --gnc-module-dir ${top_srcdir}/src/gnome-utils \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_srcdir}/src/app-utils \
  --guile-load-dir ${top_srcdir}/src/report/report-system \
  --guile-load-dir ${top_srcdir}/src/report/stylesheets \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnome \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/gnome \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${G_WRAP_LIB_DIR}

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

EXTRA_DIST = test-load-module
