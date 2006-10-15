TESTS =  \
  test-link-module
#  test-load-module

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../engine:../../gnc-module:../../calculation:.. \
   GUILE_LOAD_PATH=../../engine:..:../../gnc-module:${top_srcdir}/lib \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${top_builddir}/src/gnc-module:${top_builddir}/src/gnc-module/.libs:${top_builddir}/src/engine:${top_builddir}/src/engine/.libs:${top_builddir}/src/calculation:${top_builddir}/src/calculation/.libs:${top_builddir}/src/network-utils:${top_builddir}/src/network-utils/.libs

noinst_PROGRAMS = \
  test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD= \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ../libgncmod-register-core.la

#EXTRA_DIST = test-load-module

AM_CFLAGS = ${GLIB_CFLAGS} -I${top_srcdir}/src/test-core -I..
