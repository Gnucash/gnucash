TESTS = \
  test-load-module \
  test-escape \
  run-tests.sh

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/backend/file \
  --gnc-module-dir ${top_builddir}/src/backend/postgres \
  --guile-load-dir ${G_WRAP_MODULE_DIR}

TESTS_ENVIRONMENT := \
  TEST_DB_SOCKET_DIR=`pwd`/gnc_test_db_sock \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
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
	${top_builddir}/src/backend/postgres/libgncmod-backend-postgres.la \
	${top_builddir}/src/backend/file/libgncmod-backend-file.la \
        -lltdl

AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I.. \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS} 

EXTRA_DIST = \
  test-load-module \
  db-control.sh

