TESTS = \
  test-link-module \
  test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=${top_srcdir}/src/engine:${top_srcdir}/src/app-utils:${top_srcdir}/src/calculation:.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:..:${G_WRAP_MODULE_DIR}:${top_srcdir}/lib:${top_srcdir}/src/scm:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/gnome \
  LTDL_LIBRARY_PATH=.. \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/calculation:${top_srcdir}/src/gnome

LDADD = ../libgncmod-report-system.la 

noinst_PROGRAMS = test-link-module 

EXTRA_DIST = test-load-module
