TESTS = \
  test-link-module \
  test-load-module \
  test-exp-parser

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../engine:../../gnc-module:../../calculation:.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:../../engine:..:../../gnc-module:${top_srcdir}/lib \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/calculation:${top_srcdir}/src/calculation/.libs

LDADD = \
  -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs \
  -L${top_srcdir}/src/engine -L${top_srcdir}/src/engine/.libs \
  -L${top_srcdir}/src/calculation -L${top_srcdir}/src/calculation/.libs \
   ../../engine/libgncmod-engine.la \
   ../../gnc-module/libgncmodule.la \
   ../libgncmod-app-utils.la \
   ../../engine/libgw-engine.la \
   ${top_srcdir}/src/engine/libgw-glib.la \
   ${top_srcdir}/src/test-core/libgncmod-test.la

bin_PROGRAMS = \
  test-link-module \
  test-exp-parser

EXTRA_DIST = test-load-module

INCLUDES = \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I..

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}
