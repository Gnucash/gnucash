TESTS = \
  test-link-module \
  test-load-module \
  test-exp-parser \
  test-print-parse-amount

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../engine:../../gnc-module:../../calculation:.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:../../engine:..:../../gnc-module:${top_srcdir}/lib:${top_srcdir}/src/scm:${top_srcdir}/src/gnome \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:..:${top_srcdir}/src/gnc-module:${top_srcdir}/src/engine:${top_srcdir}/src/calculation:${top_srcdir}/src/scm:${top_srcdir}/src/gnome

LDADD = \
   ../../engine/libgncmod-engine.la \
   ../../gnc-module/libgncmodule.la \
   ../libgncmod-app-utils.la \
   ../../engine/libgw-engine.la \
   ${top_srcdir}/src/engine/libgw-glib.la \
   ${top_srcdir}/src/test-core/libgncmod-test.la \
   ${top_srcdir}/src/engine/test-core/libgncmod-test-engine.la 

noinst_PROGRAMS = \
  test-link-module \
  test-exp-parser \
  test-print-parse-amount

EXTRA_DIST = test-load-module

AM_CFLAGS = \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I.. \
  ${GLIB_CFLAGS}
