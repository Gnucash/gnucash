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
  ${GLIB_CFLAGS} \
  ${BOOST_CPPFLAGS}

LDADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/test-core/libtest-core.la \
  ../libgncmod-engine.la \
  ../test-core/libgncmod-test-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${GLIB_LIBS}

test_guid_SOURCES = test-guid.cpp

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

SCM_TESTS = \
  test-test-extras \
  test-account \
  test-split

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/engine/test \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/engine/test \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/engine/test

test_account_object_SOURCES = test-account-object.cpp
test_commodities_SOURCES = test-commodities.cpp
test_date_SOURCES = test-date.cpp
test_group_vs_book_SOURCES = test-group-vs-book.cpp
test_lots_SOURCES = test-lots.cpp
test_numeric_SOURCES = test-numeric.cpp
test_query_SOURCES = test-query.cpp
test_scm_query_SOURCES = test-scm-query.cpp
test_split_vs_account_SOURCES = test-split-vs-account.cpp
test_transaction_reversal_SOURCES = test-transaction-reversal.cpp
test_transaction_voiding_SOURCES = test-transaction-voiding.cpp

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile .scm-links
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  SRCDIR=${srcdir} \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = ${TEST_GROUP_1} ${TEST_GROUP_2}

TESTS = ${TEST_GROUP_1} test-create-account ${TEST_GROUP_2} ${SCM_TESTS}

test_link_SOURCES = test-link.c
test_link_LDADD = ../libgncmod-engine.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la

if WITH_GOOGLE_PROFILER
LDADD += -lprofiler
endif

SCM_TEST_HELPERS = test-extras.scm

EXTRA_DIST += \
  test-create-account \
  test-create-account.scm \
  test-scm-query-import \
  test-scm-query-import.scm \
  $(SCM_TEST_HELPERS) \
  $(SCM_TEST_SRCS)

TEST_PROGS += test-engine

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash/engine/test
	( cd gnucash/engine/test; for A in $(SCM_TEST_HELPERS) ; do $(LN_S) -f $(abs_srcdir)/$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

noinst_PROGRAMS = ${TEST_PROGS} ${CHECK_PROGS}

noinst_DATA: .scm-links

test_engine_SOURCES = \
	test-engine.c \
	utest-Account.cpp \
	utest-Budget.c \
	utest-Entry.c \
	utest-Invoice.c \
	test-engine-kvp-properties.c \
	utest-gnc-pricedb.c \
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
	utest-Transaction.cpp

libutest_Trans_la_LIBADD = $(LDADD)

test_import_map_SOURCES = \
        gtest-import-map.cpp
test_import_map_LDADD = \
        ${top_builddir}/src/libqof/qof/libgnc-qof.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${GLIB_LIBS} \
        ${GTEST_LIBS}

if !GOOGLE_TEST_LIBS
nodist_test_import_map_SOURCES = \
        ${GTEST_SRC}/src/gtest_main.cc
endif

test_import_map_CPPFLAGS = \
        -I${GTEST_HEADERS} \
        -I${top_srcdir}/${MODULEPATH} \
        -I${top_srcdir}/src/libqof/qof \
        -I${top_srcdir}/src/core-utils \
        ${GLIB_CFLAGS}

TEST_GROUP_1 += test-import-map


CLEANFILES = .scm-links
DISTCLEANFILES = $(SCM_TESTS)

clean-local:
	rm -f translog.*

distclean-local: clean-local
