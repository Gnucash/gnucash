TESTS=test-load-module 

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/utility-reports \
  --gnc-module-dir ${top_builddir}/src/report/stylesheets \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/lib \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/report/report-gnome \
  --guile-load-dir ${top_builddir}/src/gnome \
  --library-dir ${top_builddir}/src/gnome-utils \
  --library-dir ${top_builddir}/src/gnome

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

EXTRA_DIST = test-load-module
