
AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/test-core/libgncmod-test.la \
  ../libgncmod-engine.la \
  ../libgw-engine.la \
  ../libgw-kvp.la \
  ../test-core/libgncmod-test-engine.la \
  ${GLIB_LIBS} \
  -lltdl

TESTS = \
  test-load-engine \
  test-create-account \
  test-commodities \
  test-date \
  test-group-vs-book \
  test-object \
  test-period \
  test-querynew \
  test-query \
  test-resolve-file-path \
  test-split-vs-account  \
  test-transaction-voiding \
  test-freq-spec \
  test-scm-query \
  test-scm-query-import

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --guile-load-dir ${top_srcdir}/src/engine/test \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --library-dir    ${G_WRAP_LIB_DIR}

TESTS_ENVIRONMENT := \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-commodities \
  test-date \
  test-freq-spec \
  test-group-vs-book \
  test-load-engine \
  test-object \
  test-period \
  test-query \
  test-querynew \
  test-resolve-file-path \
  test-scm-query \
  test-split-vs-account \
  test-transaction-voiding

EXTRA_DIST = \
  test-create-account \
  test-create-account.scm \
  test-scm-query-import \
  test-scm-query-import.scm

clean:
	rm -f translog.*

distclean-local: clean
