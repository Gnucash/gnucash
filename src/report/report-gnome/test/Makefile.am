AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  $(top_builddir)/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

TESTS = \
  test-link-module \
  test-load-module

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/html \
  --gnc-module-dir ${top_builddir}/src/report/report-system \
  --gnc-module-dir ${top_builddir}/src/report/utility-reports \
  --gnc-module-dir ${top_builddir}/src/report/report-gnome \
\
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/report/report-system \
  --guile-load-dir ${top_builddir}/src/report/utility-reports \
  --guile-load-dir ${top_builddir}/src/report/report-gnome \
\
  --library-dir    ${top_builddir}/src/report/report-gnome \
  --library-dir    ${top_builddir}/src/report/report-system \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/html \
  --library-dir    ${top_builddir}/src/report/report-system \
  --library-dir    ${top_builddir}/src/report/report-gnome


TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})


check_PROGRAMS = test-link-module

EXTRA_DIST = test-load-module
