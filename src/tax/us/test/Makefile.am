TESTS = test-link-module test-load-module 

TESTS_ENVIRONMENT= \
  GNC_MODULE_PATH=.. \
  GUILE_LOAD_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/lib:..:${G_WRAP_MODULE_DIR}:${top_srcdir}/src/scm:${top_srcdir}/src/gnome:${top_srcdir}/src/engine \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/scm:${top_srcdir}/src/gnome:${top_srcdir}/src/engine

EXTRA_DIST = test-load-module

noinst_PROGRAMS=test-link-module 
test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=../libgncmod-tax-us.la 
