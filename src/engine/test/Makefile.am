INCLUDES = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I.. \
  -I../test-core \
  ${GUILE_INCS}

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} 

LDADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${top_srcdir}/src/test-core/libgncmod-test.la \
  ../libgncmod-engine.la \
  ../test-core/libgncmod-test-engine.la \
  ${GLIB_LIBS} \
  -lltdl

TESTS = \
  test-load-engine \
  test-create-account \
  test-commodities \
  test-freq-spec \
  test-resolve-file-path \
  test-split-vs-account

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="${top_srcdir}/src/engine" \
  GUILE_LOAD_PATH="${G_WRAP_MODULE_DIR}:..:../../gnc-module" \
  LTDL_LIBRARY_PATH=../../gnc-module \
  LD_LIBRARY_PATH=../../gnc-module:../../gnc-module/.libs

bin_PROGRAMS = \
  test-commodities \
  test-freq-spec \
  test-load-engine \
  test-resolve-file-path \
  test-split-vs-account
