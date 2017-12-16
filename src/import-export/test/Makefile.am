include $(top_srcdir)/test-templates/Makefile.decl

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/engine/test-core \
  ${GTK_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ../libgncmod-generic-import.la \
  $(top_builddir)/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

TESTS = \
  test-link \
  test-import-parse

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/import-export \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
\
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql  \
  --library-dir    ${top_builddir}/src/gnc-module

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link \
  test-import-parse \
  test-import-pending-matches \
  test-flat-bayes

TEST_PROGS += test-import-pending-matches

noinst_PROGRAMS = $(TEST_PROGS) $(check_PROGRAMS)

test_import_pending_matches_SOURCES = test-import-pending-matches.c

test_import_pending_matches_LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ../libgncmod-generic-import.la \
  ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ${GLIB_LIBS}

test_import_pending_matches_CFLAGS = $(AM_CPPFLAGS)

test_flat_bayes_SOURCES = test-flat-bayes.c

test_flat_bayes_LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ../libgncmod-generic-import.la \
  ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ${GLIB_LIBS}

test_flat_bayes_CFLAGS = $(AM_CPPFLAGS)

clean-local:
	rm -f translog.*

distclean-local: clean-local

EXTRA_DIST += CMakeLists.txt
