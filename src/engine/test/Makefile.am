
AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I.. \
  -I../test-core \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${top_srcdir}/src/test-core/libgncmod-test.la \
  ../libgncmod-engine.la \
  ../libgw-engine.la \
  ../libgw-kvp.la \
  ../test-core/libgncmod-test-engine.la \
  ${GLIB_LIBS} \
  -lltdl

TESTS = \
  test-load-engine \
  test-create-account \
  test-commodities \
  test-date \
  test-group-vs-book \
  test-object \
  test-period \
  test-query \
  test-resolve-file-path \
  test-split-vs-account  \
  test-transaction-voiding \
  test-freq-spec \
  test-scm-query 

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${G_WRAP_MODULE_DIR}

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-commodities \
  test-date \
  test-freq-spec \
  test-group-vs-book \
  test-load-engine \
  test-object \
  test-period \
  test-query \
  test-resolve-file-path \
  test-scm-query \
  test-split-vs-account \
  test-transaction-voiding

EXTRA_DIST = \
  test-create-account \
  test-create-account.scm
