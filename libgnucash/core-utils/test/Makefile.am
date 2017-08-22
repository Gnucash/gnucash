include $(top_srcdir)/test-templates/Makefile.decl
MODULEPATH = libgnucash/core-utils

# We add src/engine here, just to load qof.h, which
# is pulled in via test-core
AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/common \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  $(BOOST_CPPFLAGS)

LDADD = \
  ../libgnc-core-utils.la \
  ${top_builddir}/common/test-core/libtest-core.la \
  ${GLIB_LIBS} \
  ${BOOST_LDFLAGS}

# these tests are ordered kind more or less in the order
# that they should be executed, with more basic tests coming first.
#

TESTS = \
  test-gnc-glib-utils \
  test-resolve-file-path \
  test-usr-conf-dir \
  test-usr-conf-dir-invalid-home

GNC_TEST_DEPS = \
  --library-dir    ${top_builddir}/libgnucash/core-utils

TESTS_ENVIRONMENT = \
  SRCDIR=${srcdir} \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = ${TESTS}

test_gnc_glib_utils_SOURCES = \
  $(top_srcdir)/$(MODULEPATH)/gnc-glib-utils.c \
  test-gnc-glib-utils.c

test_gnc_glib_utils_LDADD = \
        ${top_builddir}/common/test-core/libtest-core.la \
        $(GLIB_LIBS)

test_gnc_glib_utils_CFLAGS = \
        ${DEFAULT_INCLUDES} \
        -I${top_srcdir}/${MODULEPATH} \
        ${GLIB_CFLAGS}

EXTRA_DIST += \
	CMakeLists.txt

clean-local:
	rm -f translog.*

distclean-local: clean-local
