TESTS =  \
  test-link-module \
  test-load-module

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:..:../../gnc-module:${top_srcdir}/lib \
   LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/network-utils


noinst_PROGRAMS = \
  test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=../libgncmod-network-utils.la

#EXTRA_DIST = test-load-module

AM_CFLAGS = \
  -I${top_srcdir}/src/test-core \
  -I.. \
  ${GLIB_CFLAGS}
