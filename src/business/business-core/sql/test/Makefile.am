SUBDIRS = .

test_dbi_business_SOURCES = \
  test-dbi-business.c \
  test-dbi-business-stuff.c

TESTS = \
  test-dbi-business

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine

TESTS_ENVIRONMENT = \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  GNC_DBD_DIR=@GNC_DBD_DIR@ \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-dbi-business

LDADD = ${top_builddir}/src/test-core/libtest-core.la \
        ${top_builddir}/src/gnc-module/libgnc-module.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/src/core-utils/libgnc-core-utils.la \
        ${top_builddir}/src/libqof/qof/libgnc-qof.la \
        ${top_builddir}/src/backend/dbi/test/libgncmod-test-dbi.la \
        ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
        ${top_builddir}/src/business/business-core/sql/libgncmod-business-backend-sql.la \
        ${top_builddir}/lib/libc/libc-missing.la

AM_CFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/backend/dbi/test \
  -I${top_srcdir}/src/backend/sql \
  -I${top_srcdir}/src/business/business-core \
  -I${top_srcdir}/src/business/business-core/sql \
  -DTEST_MYSQL_URL=\"${TEST_MYSQL_URL}\" \
  -DTEST_PGSQL_URL=\"${TEST_PGSQL_URL}\" \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS} \
  ${GCONF_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.business.sql\"
