
AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/business/business-core \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/test-core/libgncmod-test.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ../libgncmod-business-core.la \
  ${GLIB_LIBS} \
  ${QOF_LIBS} \
  -lltdl

TESTS = \
  test-load-module \
  test-business \
  test-address \
  test-customer \
  test-employee \
  test-job \
  test-vendor

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/core-utils \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/business/business-core \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --gnc-module-dir ${top_srcdir}/src/engine

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-load-module \
  test-business \
  test-address \
  test-customer \
  test-employee \
  test-job \
  test-vendor

EXTRA_DIST = 
