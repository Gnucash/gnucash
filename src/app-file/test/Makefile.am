TESTS=test-link test-load-module 

TESTS_ENVIRONMENT=\
GNC_MODULE_PATH=${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/calculation \
GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/calculation:${top_srcdir}/lib:${top_srcdir}/src/gnome:${top_srcdir}/src/scm \
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/app-file/gnome:${top_srcdir}/src/calculation:${top_srcdir}/src/gnome


noinst_PROGRAMS=test-link

test_link_SOURCES=test-link.c
test_link_LDADD=../libgncmod-app-file.la 

testit: 
	${TESTS_ENVIRONMENT} gdb guile
