
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

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="..:${top_srcdir}/src/engine" \
  GUILE_LOAD_PATH="${G_WRAP_MODULE_DIR}:..:../../../gnc-module:${top_srcdir}/src/scm" \
  LTDL_LIBRARY_PATH=${top_srcdir}/src/gnc-module:..:${top_srcdir}/src/engine \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:..:${top_srcdir}/src/engine:${top_srcdir}/src/gnc-module

noinst_PROGRAMS = \
  test-business \
  test-address \
  test-customer \
  test-vendor \
  test-employee \
  test-job

EXTRA_DIST = 
