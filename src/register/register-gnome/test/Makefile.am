TESTS =  \
  test-link-module
#  test-load-module

TESTS_ENVIRONMENT= \
   GNC_MODULE_PATH=../../engine:../../gnc-module:../../calculation:.. \
   GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:../../engine:..:../../gnc-module:${top_srcdir}/lib \
   LTDL_LIBRARY_PATH=.. \
   LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/calculation:${top_srcdir}/src/calculation/.libs:${top_srcdir}/src/network-utils:${top_srcdir}/src/network-utils/.libs

LDADD = \
  -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs \
  -L${top_srcdir}/src/engine -L${top_srcdir}/src/engine/.libs \
  -L${top_srcdir}/src/calculation -L${top_srcdir}/src/calculation/.libs \
  -L${top_srcdir}/src/network-utils -L${top_srcdir}/src/network-utils/.libs \
  -L${top_srcdir}/src/app-utils -L${top_srcdir}/src/app-utils/.libs \
  -L${top_srcdir}/src/gnome-utils -L${top_srcdir}/src/gnome-utils/.libs \
  -L${top_srcdir}/src/register/register-core \
   -L${top_srcdir}/src/register/register-core/.libs \
   ${top_srcdir}/src/gnc-module/libgncmodule.la \
   ${top_srcdir}/src/engine/libgncmod-engine.la \
   ${top_srcdir}/src/app-utils/libgncmod-app-utils.la \
   ${top_srcdir}/src/gnome-utils/libgncmod-gnome-utils.la \
   ${top_srcdir}/src/register/register-core/libgncmod-register-core.la \
   ../libgncmod-register-gnome.la \
   ${top_srcdir}/src/engine/libgw-engine.la \
   ${top_srcdir}/src/engine/libgw-glib.la \
   ${top_srcdir}/src/test-core/libgncmod-test.la \
   ${top_srcdir}/src/engine/test-core/libgncmod-test-engine.la \
  -lltdl

noinst_PROGRAMS = \
  test-link-module

#EXTRA_DIST = test-load-module

CFLAGS = @CFLAGS@ -I${top_srcdir}/src/test-core -I.. ${GLIB_CFLAGS} 

