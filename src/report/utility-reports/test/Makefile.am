TESTS=test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=${top_srcdir}/src/engine:${top_srcdir}/src/report/report-system:${top_srcdir}/src/app-utils:${top_srcdir}/src/calculation:.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/lib:..:${G_WRAP_MODULE_DIR}:${top_srcdir}/src/gnome:${top_srcdir}/src/scm \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/app-utils:${top_srcdir}/src/app-utils/.libs:${top_srcdir}/src/calculation:${top_srcdir}/src/calculation/.libs:${top_srcdir}/src/gnome:${top_srcdir}/src/gnome/.libs

EXTRA_DIST = test-load-module
