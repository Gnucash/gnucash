TESTS=test-link-module test-load-module

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../engine:../../gnc-module:.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:../../engine:..:../../gnc-module:${top_srcdir}/lib \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs

LDADD = \
   ../../engine/libgncmod-engine.la \
   ../../gnc-module/libgncmodule.la

bin_PROGRAMS=test-link-module

test-link-module: test-link-module.c


