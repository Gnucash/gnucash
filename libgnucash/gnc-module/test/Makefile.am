SUBDIRS = mod-foo mod-bar mod-baz misc-mods

LDADD = \
  ${top_builddir}/common/test-core/libtest-core.la \
  ../libgnc-module.la ${GLIB_LIBS} ${GUILE_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
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
  --gnc-module-dir ${top_builddir}/libgnucash/gnc-module \
  --gnc-module-dir ${top_builddir}/libgnucash/gnc-module/test/mod-foo \
  --gnc-module-dir ${top_builddir}/libgnucash/gnc-module/test/mod-bar \
  --gnc-module-dir ${top_builddir}/libgnucash/gnc-module/test/mod-baz \
  --gnc-module-dir ${top_builddir}/libgnucash/gnc-module/test/misc-mods \
  --guile-load-dir ${top_builddir}/common/test-core \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_srcdir}/libgnucash/gnc-module/test/mod-foo \
  --guile-load-dir ${top_srcdir}/libgnucash/gnc-module/test/mod-baz \
  --guile-load-dir ${top_srcdir}/libgnucash/gnc-module/test/mod-bar \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/common/test-core \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/gnc-module/test/mod-foo \
  --library-dir    ${top_builddir}/libgnucash/gnc-module/test/mod-bar \
  --library-dir    ${top_builddir}/libgnucash/gnc-module/test/mod-baz

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

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
  test-scm-multi \
  CMakeLists.txt

foo:
	${TESTS_ENVIRONMENT} strace -f -o xxx ./test-load-c
