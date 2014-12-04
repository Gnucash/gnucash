include $(top_srcdir)/test-templates/Makefile.decl

MODULEPATH = src/engine

AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/core-utils \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ../libgncmod-engine.la \
  ../test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${GLIB_LIBS}

# these tests are ordered kind more or less in the order
# that they should be executed, with more basic tests coming first.
#
TEST_GROUP_1 = \
  test-link \
  test-load-engine \
  test-guid \
  test-numeric \
  test-date \
  test-object \
  test-commodities \
  test-engine

TEST_GROUP_2 = \
  test-account-object \
  test-group-vs-book \
  test-lots \
  test-querynew \
  test-query \
  test-split-vs-account  \
  test-transaction-reversal \
  test-transaction-voiding \
  test-recurrence \
  test-scm-query \
  test-business \
  test-address \
  test-customer \
  test-employee \
  test-job \
  test-vendor

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  SRCDIR=${srcdir} \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${top_builddir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = ${TEST_GROUP_1} ${TEST_GROUP_2}

TESTS = ${TEST_GROUP_1} test-create-account ${TEST_GROUP_2}

test_link_SOURCES = test-link.c
test_link_LDADD = ../libgncmod-engine.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la

if WITH_GOOGLE_PROFILER
LDADD += -lprofiler
endif

EXTRA_DIST += \
  test-create-account \
  test-create-account.scm \
  test-scm-query-import \
  test-scm-query-import.scm


test_engine_SOURCES = \
	test-engine.c \
	utest-Account.c \
	utest-Budget.c \
	utest-Entry.c \
	utest-Invoice.c \
	test-engine-kvp-properties.c \
	dummy.cpp

test_engine_LDADD = \
	libutest-Split.la \
	libutest-Trans.la \
	$(LDADD)
test_engine_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I${top_srcdir}/${MODULEPATH}/ \
	-DTESTPROG=test_engine \
	${GLIB_CFLAGS} \
	${AM_CFLAGS}

noinst_LTLIBRARIES = \
	libutest-Split.la \
	libutest-Trans.la

libutest_Split_la_SOURCES = \
	utest-Split.cpp \
	${top_srcdir}/src/libqof/qof/gnc-numeric.cpp
libutest_Split_la_CXXFLAGS = -Wno-write-strings $(AM_CXXFLAGS)

libutest_Split_la_LIBADD = $(LDADD)

libutest_Trans_la_SOURCES = \
	utest-Transaction.c

libutest_Trans_la_LIBADD = $(LDADD)


clean-local:
	rm -f translog.*

distclean-local: clean-local
