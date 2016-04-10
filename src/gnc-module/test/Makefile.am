SUBDIRS = mod-foo mod-bar mod-baz misc-mods

LDADD = \
  ${top_builddir}/src/test-core/libtest-core.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ../libgnc-module.la ${GLIB_LIBS} ${GUILE_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/test-core \
  ${GUILE_CFLAGS} \
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

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/mod-foo \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/mod-bar \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/mod-baz \
  --gnc-module-dir ${top_builddir}/src/gnc-module/test/misc-mods \
  --guile-load-dir ${top_builddir}/src/test-core \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_srcdir}/src/gnc-module/test/mod-foo \
  --guile-load-dir ${top_srcdir}/src/gnc-module/test/mod-baz \
  --guile-load-dir ${top_srcdir}/src/gnc-module/test/mod-bar \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/test-core \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/gnc-module/test/mod-foo \
  --library-dir    ${top_builddir}/src/gnc-module/test/mod-bar \
  --library-dir    ${top_builddir}/src/gnc-module/test/mod-baz

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-load-c \
  test-modsysver \
  test-incompatdep \
  test-agedver \
  test-dynload

test_dynload_LDFLAGS = ${GUILE_LIBS}

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
