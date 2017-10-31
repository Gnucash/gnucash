include $(top_srcdir)/test-templates/Makefile.decl

MODULEPATH = libgnucash/engine

AM_CPPFLAGS = \
  -I${top_srcdir} \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/engine/test-core \
  -I${top_srcdir}/libgnucash/core-utils \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${BOOST_CPPFLAGS}

LDADD = \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/common/test-core/libtest-core.la \
  ../libgncmod-engine.la \
  ../test-core/libgncmod-test-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${GLIB_LIBS}

test_guid_SOURCES = test-guid.cpp

# these tests are ordered kind more or less in the order
# that they should be executed, with more basic tests coming first.
#
TEST_GROUP_1 = \
  test-link \
  test-load-engine \
  test-guid \
  test-date \
  test-object \
  test-commodities \
  test-qof \
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
  test-create-account \
  test-test-extras \
  test-account \
  test-split
#  test-scm-query-import \ Fails

SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/engine/test \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/engine/test \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/libgnucash/engine/test

test_account_object_SOURCES = test-account-object.cpp
test_commodities_SOURCES = test-commodities.cpp
test_date_SOURCES = test-date.cpp
test_group_vs_book_SOURCES = test-group-vs-book.cpp
test_lots_SOURCES = test-lots.cpp

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
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = ${TEST_GROUP_1} ${TEST_GROUP_2}

TESTS = ${TEST_GROUP_1} ${TEST_GROUP_2} ${SCM_TESTS}

test_link_SOURCES = test-link.c
test_link_LDADD = ../libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la

if WITH_GOOGLE_PROFILER
LDADD += -lprofiler
endif

SCM_TEST_HELPERS = test-extras.scm

EXTRA_DIST += \
  $(SCM_TEST_HELPERS) \
  $(SCM_TEST_SRCS) \
  CMakeLists.txt

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

test_qof_SOURCES = \
	test-gnc-date.c \
	test-qof.c \
	test-qofbook.c \
	test-qofinstance.cpp \
	test-qofobject.c \
	test-qof-string-cache.c

test_qof_LDADD = \
	$(LDADD)
test_qof_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I${top_srcdir}/${MODULEPATH}/ \
	-DTESTPROG=test_qof \
	${GLIB_CFLAGS} \
	${AM_CFLAGS}

test_engine_SOURCES = \
	test-engine.c \
	test-gnc-uri-utils.c \
	utest-Account.cpp \
	utest-Budget.c \
	utest-Entry.c \
	utest-Invoice.c \
	test-engine-kvp-properties.c \
	utest-gnc-pricedb.c \
	utest-Transaction.cpp \
	dummy.cpp \
	utest-Split.cpp \
	${top_srcdir}/${MODULEPATH}/gnc-numeric.cpp

test_engine_LDADD = \
	$(LDADD)
test_engine_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I${top_srcdir}/${MODULEPATH}/ \
	-DTESTPROG=test_engine \
	${GLIB_CFLAGS} \
	${AM_CFLAGS}
test_engine_CXXFLAGS = -Wno-write-strings $(AM_CXXFLAGS)

test_import_map_SOURCES = \
        gtest-import-map.cpp
test_import_map_LDADD = \
        ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
        ${GLIB_LIBS} \
        ${GTEST_LIBS}

if !GOOGLE_TEST_LIBS
nodist_test_import_map_SOURCES = \
        ${GTEST_SRC}/src/gtest_main.cc
endif

test_import_map_CPPFLAGS = \
        -I${GTEST_HEADERS} \
        -I${top_srcdir}/${MODULEPATH} \
        -I${top_srcdir}/libgnucash/core-utils \
        ${GLIB_CFLAGS}

TEST_GROUP_1 += test-import-map


CLEANFILES = .scm-links
DISTCLEANFILES = $(SCM_TESTS)

clean-local:
	rm -f translog.*

distclean-local: clean-local
