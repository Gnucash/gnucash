TESTS=test-link test-load-module 

TESTS_ENVIRONMENT=\
GNC_MODULE_PATH=${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine \
GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine \
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${top_srcdir}/src/gnc-module:${top_srcdir}/src/app-file:${top_srcdir}/src/app-utils:${top_srcdir}/src/engine:${top_srcdir}/src/app-file/gnome


noinst_PROGRAMS=test-link

test_link_SOURCES=test-link.c
test_link_LDADD=../libgncmod-app-file.la 

testit: 
	${TESTS_ENVIRONMENT} gdb guile
