
AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/core-utils \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

LDADD = \
  ../libgnc-core-utils.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLIB_LIBS}

# these tests are ordered kind more or less in the order
# that they should be executed, with more basic tests coming first.
#
TESTS = \
  test-resolve-file-path \
  test-resolve-url

GNC_TEST_DEPS = \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils

TESTS_ENVIRONMENT = \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-resolve-file-path \
  test-resolve-url


test_link_SOURCES = test-link.c
test_link_LDADD = ../libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la

EXTRA_DIST =

clean-local:
	rm -f translog.*

distclean-local: clean-local
