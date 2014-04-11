SUBDIRS = mod-foo mod-bar mod-baz misc-mods

LDADD = ../libgncmodule.la ${GLIB_LIBS} -lltdl ${GUILE_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

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

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/mod-foo \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/mod-bar \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/mod-baz \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/misc-mods \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --gnc-module-dir ${top_srcdir}/src/gnc-module/test/mod-foo \
  --gnc-module-dir ${top_srcdir}/src/gnc-module/test/mod-bar \
  --gnc-module-dir ${top_srcdir}/src/gnc-module/test/mod-baz \
  --gnc-module-dir ${top_srcdir}/src/gnc-module/test/misc-mods

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

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
  test-load-scm \
  test-scm-dynload \
  test-scm-init \
  test-scm-module \
  test-scm-multi

foo:
	${TESTS_ENVIRONMENT} strace -f -o xxx ./test-load-c
