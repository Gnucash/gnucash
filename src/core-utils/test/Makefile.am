
AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/core-utils \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ../libgnc-core-utils.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ${GLIB_LIBS}

# these tests are ordered kind more or less in the order
# that they should be executed, with more basic tests coming first.
#
TESTS = \
  test-gnc-uri-utils \
  test-resolve-file-path

GNC_TEST_DEPS = \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils

TESTS_ENVIRONMENT = \
  SRCDIR=${srcdir} \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-gnc-uri-utils \
  test-resolve-file-path


EXTRA_DIST =

clean-local:
	rm -f translog.*

distclean-local: clean-local
