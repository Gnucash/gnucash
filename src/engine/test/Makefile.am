
AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I.. \
  -I../test-core \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${top_srcdir}/src/test-core/libgncmod-test.la \
  ../libgncmod-engine.la \
  ../libgw-engine.la \
  ../libgw-glib.la \
  ../libgw-kvp.la \
  ../test-core/libgncmod-test-engine.la \
  ${GLIB_LIBS} \
  -lltdl

TESTS = \
  test-load-engine \
  test-create-account \
  test-commodities \
  test-group-vs-book \
  test-query \
  test-resolve-file-path \
  test-split-vs-account  \
  test-transaction-voiding 

#  test-freq-spec 
#  test-scm-query 

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="${top_srcdir}/src/engine" \
  GUILE_LOAD_PATH="${G_WRAP_MODULE_DIR}:..:../../gnc-module:${top_srcdir}/src/scm" \
  LTDL_LIBRARY_PATH=../../gnc-module \
  LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:..:${top_srcdir}/src/gnc-module

noinst_PROGRAMS = \
  test-commodities \
  test-freq-spec \
  test-group-vs-book \
  test-load-engine \
  test-query \
  test-resolve-file-path \
  test-scm-query \
  test-split-vs-account \
  test-transaction-voiding

EXTRA_DIST = \
  test-create-account \
  test-create-account.scm
