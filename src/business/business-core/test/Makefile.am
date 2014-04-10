
AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I.. \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${top_srcdir}/src/test-core/libgncmod-test.la \
  ${top_srcdir}/src/engine/libgncmod-engine.la \
  ${top_srcdir}/src/engine/libgw-engine.la \
  ../libgncmod-business-core.la \
  ../libgw-business-core.la \
  ${GLIB_LIBS} \
  -lltdl

TESTS = \
  test-business \
  test-address \
  test-customer \
  test-vendor \
  test-employee \
  test-job

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/business/business-core \
  --guile-load-dir ${G_WRAP_MODULE_DIR}

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-business \
  test-address \
  test-customer \
  test-vendor \
  test-employee \
  test-job

EXTRA_DIST = 
