TESTS = \
  test-link-module \
  test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=${top_srcdir}/src/engine:${top_srcdir}/src/app-utils:${top_srcdir}/src/calculation:.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:..:${G_WRAP_MODULE_DIR}:${top_srcdir}/lib:${top_srcdir}/src/scm:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/guile \
  LTDL_LIBRARY_PATH=.. \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/app-utils:${top_srcdir}/src/app-utils/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/calculation:${top_srcdir}/src/calculation/.libs:${top_srcdir}/src/guile:${top_srcdir}/src/guile/.libs

LDADD = \
  -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs \
  ../libgncmod-report-system.la 

bin_PROGRAMS = test-link-module 

EXTRA_DIST = test-load-module
