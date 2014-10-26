TESTS =  \
  test-link-module test-load-module

# The following tests are nice, but have absolutely no place in an
# automated testing system.
#
TESTS_GUI = test-gnc-recurrence

##lib_LTLIBRARIES = libgncgnome.la

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/app-utils


TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  SRCDIR=${srcdir} \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${top_builddir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link-module test-gnc-recurrence

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GUILE_CFLAGS}

LDADD = \
  ${GNOME_LIBS} \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/test-core/libtest-core.la

test_gnc_recurrence_SOURCES=test-gnc-recurrence.c
test_gnc_recurrence_LDADD = \
  ${GTK_LIBS} \
  ${LDADD}

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${top_builddir}/src/gnc-module/libgnc-module.la

EXTRA_DIST = \
  test-load-module

testit:
	libtool --mode execute gdb test-link-module
