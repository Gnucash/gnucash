SUBDIRS = .

include $(top_srcdir)/test-templates/Makefile.decl
MODULEPATH = src/backend/sql

test_column_types_SOURCES = \
  test-column-types.c

check_PROGRAMS = \
  test-column-types \
  test-sqlbe

TESTS = ${check_PROGRAMS}

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/sql

TESTS_ENVIRONMENT = \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

LDADD = ${top_builddir}/src/test-core/libtest-core.la \
        ${top_builddir}/src/gnc-module/libgnc-module.la \
        ${top_builddir}/src/backend/sql/libgnc-backend-sql.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/src/core-utils/libgnc-core-utils.la \
        ${top_builddir}/src/libqof/qof/libgnc-qof.la \
        ${top_builddir}/lib/libc/libc-missing.la

AM_CPPFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/backend/sql \
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.backend.sql\"

#test_sqlbedir = ${top_srcdir}/${MODULEPATH}/test

test_sqlbe_SOURCES = \
	test-sqlbe.c \
	utest-gnc-backend-sql.c

#test_sqlbe_HEADERS = \
#	$(top_srcdir)/$(MODULEPATH)/gnc-backend-sql.h

test_sqlbe_LDADD = \
	$(top_builddir)/$(MODULEPATH)/libgnc-backend-sql.la \
	$(top_builddir)/src/libqof/qof/libgnc-qof.la \
	$(top_builddir)/src/test-core/libtest-core.la \
	$(top_builddir)/src/core-utils/libgnc-core-utils.la \
	$(GLIB_LIBS)

test_sqlbe_CFLAGS = \
	-DTESTPROG=test_sqlbe \
	$(DEFAULT_INCLUDES) \
	-I$(top_srcdir)/$(MODULEPATH)/ \
	-I$(top_srcdir)/src/backend/dbi/ \
	-I$(top_srcdir)/src/libqof/qof/ \
	-I$(top_srcdir)/src/test-core/ \
	$(GLIB_CFLAGS)
