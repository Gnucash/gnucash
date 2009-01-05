TESTS = \
  test-load-backend \
  test-escape \
  run-tests.sh

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_srcdir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/backend/xml \
  --gnc-module-dir ${top_builddir}/src/backend/postgres

TESTS_ENVIRONMENT = \
  TEST_DB_SOCKET_DIR=`pwd`/gnc_test_db_sock \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-load-backend \
  test-db \
  test-escape \
  test-period

LDADD = ${top_builddir}/src/test-core/libgncmod-test.la \
        ${top_builddir}/src/gnc-module/libgnc-module.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
        -lpq

test_db_SOURCES = \
  ${top_srcdir}/src/backend/postgres/PostgresBackend.c \
  ${top_srcdir}/src/backend/postgres/account.c \
  ${top_srcdir}/src/backend/postgres/book.c \
  ${top_srcdir}/src/backend/postgres/builder.c \
  ${top_srcdir}/src/backend/postgres/checkpoint.c \
  ${top_srcdir}/src/backend/postgres/escape.c \
  ${top_srcdir}/src/backend/postgres/events.c \
  ${top_srcdir}/src/backend/postgres/gncquery.c \
  ${top_srcdir}/src/backend/postgres/kvp-sql.c \
  ${top_srcdir}/src/backend/postgres/price.c \
  ${top_srcdir}/src/backend/postgres/txn.c \
  ${top_srcdir}/src/backend/postgres/txnmass.c \
  ${top_srcdir}/src/backend/postgres/upgrade.c \
  ${top_srcdir}/src/backend/postgres/putil.c \
  test-db.c

test_escape_SOURCES = \
  ${top_srcdir}/src/backend/postgres/PostgresBackend.c \
  ${top_srcdir}/src/backend/postgres/account.c \
  ${top_srcdir}/src/backend/postgres/book.c \
  ${top_srcdir}/src/backend/postgres/builder.c \
  ${top_srcdir}/src/backend/postgres/checkpoint.c \
  ${top_srcdir}/src/backend/postgres/escape.c \
  ${top_srcdir}/src/backend/postgres/events.c \
  ${top_srcdir}/src/backend/postgres/gncquery.c \
  ${top_srcdir}/src/backend/postgres/kvp-sql.c \
  ${top_srcdir}/src/backend/postgres/price.c \
  ${top_srcdir}/src/backend/postgres/txn.c \
  ${top_srcdir}/src/backend/postgres/txnmass.c \
  ${top_srcdir}/src/backend/postgres/upgrade.c \
  ${top_srcdir}/src/backend/postgres/putil.c \
  test-escape.c

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  ${QOF_CFLAGS} \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I.. \
  -I${srcdir}/.. \
  ${PGSQL_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS} 

EXTRA_DIST = \
  db-control.sh \
  run-tests.sh

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.pg\"
