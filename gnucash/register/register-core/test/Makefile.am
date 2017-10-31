TESTS =  \
  test-link-module

#GNC_TEST_DEPS := \
#

#TESTS_ENVIRONMENT := \
#  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=\
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ../libgncmod-register-core.la

EXTRA_DIST = CMakeLists.txt

AM_CPPFLAGS = ${GLIB_CFLAGS} -I${top_srcdir}/common/test-core -I..
