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
  test-modsysver \
  test-incompatdep \
  test-agedver \
  test-dynload \
  ${SCM_TESTS}

SCM_TESTS = \
  test-gnc-module-load-scm \
  test-gnc-module-swigged-c \
  test-gnc-module-scm-module \
  test-gnc-module-scm-multi \
  test-gnc-module-load-deps \
  test-gnc-module-scm-dynload \
  test-gnc-module-scm-init

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

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

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@

EXTRA_DIST = \
  ${SCM_TEST_SRCS} \
  CMakeLists.txt

foo:
	${TESTS_ENVIRONMENT} strace -f -o xxx ./test-load-c
