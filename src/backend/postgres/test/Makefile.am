TESTS = \
  test-load-backend \
  test-escape \
  run-tests.sh

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_srcdir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/backend/file \
  --gnc-module-dir ${top_builddir}/src/backend/postgres \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --library-dir    ${G_WRAP_LIB_DIR}

TESTS_ENVIRONMENT := \
  TEST_DB_SOCKET_DIR=`pwd`/gnc_test_db_sock \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-load-backend \
  test-db \
  test-escape \
  test-period

LDADD = ${top_builddir}/src/test-core/libgncmod-test.la \
        ${top_builddir}/src/gnc-module/libgncmodule.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/libgw-engine.la \
        ${top_builddir}/src/engine/libgw-kvp.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
	${top_builddir}/src/backend/postgres/libgnc-backend-postgres.la \
        -lltdl

AM_CFLAGS = \
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
