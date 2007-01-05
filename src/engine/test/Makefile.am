
AM_CFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} 

LDADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/test-core/libgncmod-test.la \
  ../libgncmod-engine.la \
  ../test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${GLIB_LIBS} \
  ${QOF_LIBS} \
  -lltdl

# these tests are ordered kind more or less in the order
# that they should be executed, with more basic tests coming first.
# 
TESTS = \
  test-link \
  test-load-engine \
  test-guid \
  test-numeric \
  test-date \
  test-object \
  test-commodities \
  test-create-account \
  test-group-vs-book \
  test-lots \
  test-period \
  test-querynew \
  test-query \
  test-recursive \
  test-resolve-file-path \
  test-split-vs-account  \
  test-transaction-reversal \
  test-transaction-voiding \
  test-freq-spec \
  test-recurrence \
  test-scm-query \
  test-book-merge 

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_srcdir}/src/gnc-module \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/backend/file \
  --guile-load-dir ${top_srcdir}/src/engine/test

TESTS_ENVIRONMENT := \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link \
  test-commodities \
  test-date \
  test-freq-spec \
  test-recurrence \
  test-guid \
  test-group-vs-book \
  test-load-engine \
  test-period \
  test-lots \
  test-numeric \
  test-book-merge \
  test-object \
  test-query \
  test-querynew \
  test-recursive \
  test-resolve-file-path \
  test-scm-query \
  test-split-vs-account \
  test-transaction-reversal \
  test-transaction-voiding


test_link_SOURCES = test-link.c
test_link_LDADD = ../libgncmod-engine.la ${QOF_LIBS}

EXTRA_DIST = \
  test-create-account \
  test-create-account.scm \
  test-scm-query-import \
  test-scm-query-import.scm

clean-local:
	rm -f translog.*

distclean-local: clean-local
