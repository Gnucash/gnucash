TESTS = \
  test-link-module \
  test-load-module \
  test-component-manager \
  test-exp-parser \
  test-scm-query-string \
  test-print-parse-amount

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --gnc-module-dir ${top_srcdir}/src/app-utils \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_srcdir}/lib \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnome \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_srcdir}/src/gnome \
  --library-dir    ${G_WRAP_LIB_DIR}

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

LDADD = \
   ../../engine/libgncmod-engine.la \
   ../../gnc-module/libgncmodule.la \
   ../libgncmod-app-utils.la \
   ../../engine/libgw-engine.la \
   ${top_builddir}/src/test-core/libgncmod-test.la \
   ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
   ${GUILE_LIBS}

noinst_PROGRAMS = \
  test-link-module \
  test-exp-parser \
  test-print-parse-amount \
  test-scm-query-string \
  test-print-queries

EXTRA_DIST = \
  test-load-module \
  test-component-manager

AM_CFLAGS = \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}
