TESTS = test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/lib:..:${G_WRAP_MODULE_DIR}:${top_srcdir}/src/scm:${top_srcdir}/src/guile \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/scm:${top_srcdir}/src/guile

EXTRA_DIST = test-load-module
