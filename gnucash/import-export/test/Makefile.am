include $(top_srcdir)/test-templates/Makefile.decl

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/import-export \
  -I${top_srcdir}/libgnucash/engine/test-core \
  ${GTK_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/common/test-core/libtest-core.la \
  ../libgncmod-generic-import.la \
  $(top_builddir)/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

TESTS = \
  test-link \
  test-import-parse \
  test-import-pending-matches

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/import-export \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
\
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql  \
  --library-dir    ${top_builddir}/libgnucash/gnc-module

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link \
  test-import-parse \
  test-import-pending-matches

noinst_PROGRAMS = $(TEST_PROGS) $(check_PROGRAMS)

test_import_pending_matches_SOURCES = test-import-pending-matches.cpp

test_import_pending_matches_LDADD = \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ../libgncmod-generic-import.la \
  ${top_builddir}/libgnucash/engine/test-core/libgncmod-test-engine.la \
  ${top_builddir}/common/test-core/libtest-core.la \
  ${GLIB_LIBS}

test_import_pending_matches_CFLAGS = $(AM_CPPFLAGS)

clean-local:
	rm -f translog.*

distclean-local: clean-local

EXTRA_DIST += CMakeLists.txt
