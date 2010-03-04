SUBDIRS = .

test_column_types_SOURCES = \
  ${top_srcdir}/src/backend/sql/gnc-backend-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-account-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-book-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-budget-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-commodity-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-lots-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-price-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-recurrence-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-schedxaction-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-slots-sql.c \
  ${top_srcdir}/src/backend/sql/gnc-transaction-sql.c \
  ${top_srcdir}/src/backend/sql/escape.c \
  test-column-types.c

TESTS = \
  test-column-types

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/sql

TESTS_ENVIRONMENT = \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-column-types

#noinst_HEADERS = test-file-stuff.h

LDADD = ${top_builddir}/src/test-core/libtest-core.la \
        ${top_builddir}/src/gnc-module/libgnc-module.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/src/core-utils/libgnc-core-utils.la \
        ${top_builddir}/src/libqof/qof/libgnc-qof.la \
        ${top_builddir}/lib/libc/libc-missing.la

AM_CFLAGS = \
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
  ${GUILE_INCS} \
  ${GCONF_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.sql\"
