SUBDIRS = mod-foo mod-bar mod-baz misc-mods

LDADD = ../libgncmodule.la ${GLIB_LIBS} -lltdl

AM_CFLAGS = ${GLIB_CFLAGS} -I../ ${GUILE_INCS}

TESTS = \
  test-load-c \
  test-load-scm \
  test-gwrapped-c \
  test-scm-module \
  test-scm-multi \
  test-load-deps \
  test-modsysver \
  test-incompatdep \
  test-agedver \
  test-dynload \
  test-scm-dynload \
  test-scm-init 

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH=${PWD}/mod-foo:${PWD}/mod-bar:${PWD}/mod-baz:${PWD}/misc-mods \
  GUILE_LOAD_PATH=..:${G_WRAP_MODULE_DIR} \
  LTDL_LIBRARY_PATH=../ \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:mod-foo:mod-foo/.libs:mod-bar:mod-bar/.libs:mod-baz:mod-baz/.libs:..:../.libs

noinst_PROGRAMS = \
  test-load-c \
  test-modsysver \
  test-incompatdep \
  test-agedver \
  test-dynload

test_dynload_LDFLAGS = -lltdl ${GUILE_LIBS}

EXTRA_DIST = \
  test-gwrapped-c \
  test-load-deps \
  test-load-deps.scm \
  test-load-scm \
  test-scm-dynload \
  test-scm-init \
  test-scm-module \
  test-scm-multi \
  test-scm-multi.scm
