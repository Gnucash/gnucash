TESTS =  \
  test-link-module
#  test-load-module

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../engine:../../gnc-module:../../calculation:.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:../../engine:..:../../gnc-module:${top_srcdir}/lib \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/calculation:${top_srcdir}/src/calculation/.libs:${top_srcdir}/src/network-utils:${top_srcdir}/src/network-utils/.libs

noinst_PROGRAMS = \
  test-link-module

test_link_module_SOURCES = test-link-module.c
test_link_module_LDADD = ../libgncmod-register-gnome.la 

#EXTRA_DIST = test-load-module

AM_CFLAGS = -I${top_srcdir}/src/test-core -I.. ${GLIB_CFLAGS} 

