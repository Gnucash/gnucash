TESTS =  \
  test-link-module test-load-module 

TESTS_ENVIRONMENT=\
GNC_MODULE_PATH=${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/gnome-utils:${top_srcdir}/src/network-utils:${top_srcdir}/src/engine:${top_srcdir}/src/calculation:${top_srcdir}/src/tax/us:${top_srcdir}/src/report/locale-specific/us \
GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/gnome-utils:${top_srcdir}/src/network-utils:${top_srcdir}/src/engine:${top_srcdir}/src/calculation:${top_srcdir}/lib:${top_srcdir}/src/gnome:${top_srcdir}/src/scm:${top_srcdir}/src/report/report-system:${top_srcdir}/src/tax/us:${G_WRAP_MODULE_DIR}:${top_srcdir}/src/report/locale-specific/us \
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/gnome-utils:${top_srcdir}/src/network-utils:${top_srcdir}/src/engine:${top_srcdir}/src/app-file/gnome:${top_srcdir}/src/calculation:${top_srcdir}/src/gnome:${top_srcdir}/src/report/report-system:${top_srcdir}/src/report/locale-specific/us:${top_srcdir}/src/tax/us

noinst_PROGRAMS = \
  test-link-module

INCLUDES=-I../../gnc-module ${GLIB_CFLAGS} ${GUILE_INCS}

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD= \
../../gnc-module/libgncmodule.la 


testit:
	libtool --mode execute gdb test-link-module


