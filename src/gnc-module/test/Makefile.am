SUBDIRS=mod-foo mod-bar mod-baz misc-mods

LDADD= ../libgncmodule.la ${GLIB_LIBS} -lltdl
CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} -I../

TESTS=test-load-c test-load-scm test-gwrapped-c test-scm-module \
test-scm-multi test-load-deps test-modsysver test-incompatdep test-agedver \
test-dynload test-scm-dynload test-scm-init 

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH=${PWD}/mod-foo:${PWD}/mod-bar:${PWD}/mod-baz:${PWD}/misc-mods \
  GUILE_LOAD_PATH=..:${G_WRAP_MODULE_DIR} \
  LTDL_LIBRARY_PATH=../ \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:mod-foo:mod-foo/.libs:mod-bar:mod-bar/.libs:mod-baz:mod-baz/.libs

noinst_PROGRAMS=test-load-c test-modsysver test-incompatdep test-agedver \
test-dynload

test_load_c_SOURCES=test-load-c.c

test_modsysver_SOURCES=test-modsysver.c

test_incompatdep_SOURCES=test-incompatdep.c

test_agedver_SOURCES=test-agedver.c

test_dynload_SOURCES=test-dynload.c
test_dynload_LDFLAGS=-lltdl ${GUILE_LIBS}
