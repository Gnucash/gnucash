AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/import-export \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/test-core/libgncmod-test.la \
  ../libgncmod-generic-import.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${GLIB_LIBS} ${QOF_LIBS}

TESTS = \
  test-link \
  test-import-parse

GNC_TEST_DEPS := @GNC_TEST_SRFI_LOAD_CMD@ \
  --gnc-module-dir ${top_builddir}/src/core-utils \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/import-export \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --gnc-module-dir ${top_srcdir}/src/app-utils \
  --gnc-module-dir ${top_srcdir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/network-utils \
  --gnc-module-dir ${top_builddir}/src/gnome \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${G_WRAP_LIB_DIR} \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/import-export

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-link \
  test-import-parse
