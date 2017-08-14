SUBDIRS = .

include $(top_srcdir)/test-templates/Makefile.decl
MODULEPATH = libgnucash/backend/sql

test_column_types_SOURCES = \
  test-column-types.cpp

check_PROGRAMS = \
  test-column-types \
  test-sqlbe

TESTS = ${check_PROGRAMS}

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/sql

TESTS_ENVIRONMENT = \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

LDADD = ${top_builddir}/common/test-core/libtest-core.la \
        ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
        ${top_builddir}/libgnucash/backend/sql/libgnc-backend-sql.la \
        ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
        ${top_builddir}/libgnucash/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
        ${top_builddir}/lib/libc/libc-missing.la

AM_CPPFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/engine/test-core \
  -I${top_srcdir}/libgnucash/backend/sql \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.backend.sql\"

#test_sqlbedir = ${top_srcdir}/${MODULEPATH}/test

test_sqlbe_SOURCES = \
	test-sqlbe.cpp \
	utest-gnc-backend-sql.cpp

test_sqlbe_LDADD = \
	$(top_builddir)/$(MODULEPATH)/libgnc-backend-sql.la \
	$(top_builddir)/libgnucash/engine/libgncmod-engine.la \
	$(top_builddir)/common/test-core/libtest-core.la \
	$(top_builddir)/libgnucash/core-utils/libgnc-core-utils.la \
	$(GLIB_LIBS)

test_sqlbe_CFLAGS = \
	-DTESTPROG=test_sqlbe \
	$(DEFAULT_INCLUDES) \
	-I$(top_srcdir)/$(MODULEPATH)/ \
	-I$(top_srcdir)/libgnucash/backend/dbi/ \
	-I$(top_srcdir)/libgnucash/engine/ \
	-I$(top_srcdir)/common/test-core/ \
	$(GLIB_CFLAGS)

EXTRA_DIST += CMakeLists.txt
