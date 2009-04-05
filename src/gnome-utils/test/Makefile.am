TESTS =  \
  test-link-module test-load-module

# The following tests are nice, but have absolutely no place in an
# automated testing system.
#
TESTS_GUI = test-gnc-recurrence test-gnc-dialog

##lib_LTLIBRARIES = libgncgnome.la

GNC_TEST_DEPS = @GNC_TEST_SRFI_LOAD_CMD@ \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnome-utils

TESTS_ENVIRONMENT = \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link-module test-gnc-recurrence test-gnc-dialog

INCLUDES= \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/test-core \
  ${GLIB_CFLAGS} ${GUILE_INCS} ${GNOME_CFLAGS} ${GLADE_CFLAGS} ${QOF_CFLAGS}

LDADD = \
  ${GNOME_LIBS} \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/test-core/libgncmod-test.la

test_gnc_recurrence_SOURCES=test-gnc-recurrence.c

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD = \
  ${GUILE_LIBS} \
  ${top_builddir}/src/gnc-module/libgnc-module.la

EXTRA_DIST = \
  test-load-module

testit:
	libtool --mode execute gdb test-link-module
