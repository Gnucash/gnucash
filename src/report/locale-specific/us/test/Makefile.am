TESTS=test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=${top_srcdir}/src/engine:${top_srcdir}/src/report/report-system:${top_srcdir}/src/app-utils:${top_srcdir}/src/calculation:${top_srcdir}/src/tax/us:.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/lib:..:${G_WRAP_MODULE_DIR} \
  LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/app-utils:${top_srcdir}/src/app-utils/.libs:${top_srcdir}/src/calculation:${top_srcdir}/src/calculation/.libs

EXTRA_DIST = test-load-module
