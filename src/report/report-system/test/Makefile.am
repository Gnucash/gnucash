TESTS=test-link-module test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=${top_srcdir}/src/engine:${top_srcdir}/src/app-utils:.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:..:${G_WRAP_MODULE_DIR}:${top_srcdir}/lib:${top_srcdir}/src/scm:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine \
  LTDL_LIBRARY_PATH=.. \
  LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/app-utils:${top_srcdir}/src/app-utils/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs


bin_PROGRAMS=test-link-module 

LDADD = \
  ../libgncmod-report-system.la 

test-link-module: test-link-module.c
