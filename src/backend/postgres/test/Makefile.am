TESTS = \
  test-load-module \
  test-escape \
  run-tests.sh

TESTS_ENVIRONMENT=\
  GNC_MODULE_PATH=${top_srcdir}/src/engine::${top_srcdir}/src/backend/file:${top_srcdir}/src/backend/postgres:${top_srcdir}/src/guile:${top_srcdir}/src/scm \
  GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:..:${top_srcdir}/src/gnc-module \
  LTDL_LIBRARY_PATH=${top_srcdir}/src/gnc-module \
  LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/guile:${top_srcdir}/src/guile/.libs

noinst_PROGRAMS = \
  test-db \
  test-escape

LDADD = -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs \
        -L${top_srcdir}/src/engine -L${top_srcdir}/src/engine/.libs \
        ${top_srcdir}/src/test-core/libgncmod-test.la \
        ${top_srcdir}/src/gnc-module/libgncmodule.la \
        ${top_srcdir}/src/engine/libgncmod-engine.la \
        ${top_srcdir}/src/engine/libgncmod-engine.la \
        ${top_srcdir}/src/engine/libgw-engine.la \
        ${top_srcdir}/src/engine/libgw-glib.la \
        ${top_srcdir}/src/engine/libgw-kvp.la \
        ${top_srcdir}/src/engine/test-core/libgncmod-test-engine.la \
	${top_srcdir}/src/backend/postgres/libgncmod-backend-postgres.la \
	${top_srcdir}/src/backend/file/libgncmod-backend-file.la \
        -lltdl

CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I.. \
  ${GLIB_CFLAGS}

EXTRA_DIST = \
  db-control.sh

