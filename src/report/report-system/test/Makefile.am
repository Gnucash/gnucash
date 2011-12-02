AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GLIB_LIBS}

TESTS = \
  test-link-module \
  test-load-module

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/html \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
\
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
\
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/calculation \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql  \
  --library-dir    ${top_builddir}/src/gnc-module

TESTS_ENVIRONMENT = \
  GNC_BUILDDIR=`\cd ${top_builddir} && pwd` \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})


check_PROGRAMS = test-link-module

EXTRA_DIST = test-load-module

testit:
	$(TESTS_ENVIRONMENT) libtool --mode execute gdb test-link-module