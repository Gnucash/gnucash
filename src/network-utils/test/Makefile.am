TESTS =  \
  test-link-module
#  test-load-module

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../gnc-module:.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:..:../../gnc-module:${top_srcdir}/lib \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs

LDADD = \
  -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs \
   ${top_srcdir}/src/gnc-module/libgncmodule.la \
   ../libgncmod-network-utils.la \
  -lltdl

noinst_PROGRAMS = \
  test-link-module

#EXTRA_DIST = test-load-module

AM_CFLAGS = \
  -I${top_srcdir}/src/test-core \
  -I.. \
  ${GLIB_CFLAGS}
