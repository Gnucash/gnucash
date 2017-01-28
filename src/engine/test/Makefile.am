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
TESTS = \
  test-link \
  test-load-engine \
  test-guid \
  test-numeric \
  test-date \
  test-object \
  test-commodities \
  test-create-account \
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
  test-vendor \
  $(SCM_TESTS)

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

check_PROGRAMS = \
  test-link \
  test-commodities \
  test-date \
  test-recurrence \
  test-guid \
  test-account-object \
  test-group-vs-book \
  test-load-engine \
  test-lots \
  test-numeric \
  test-object \
  test-query \
  test-querynew \
  test-scm-query \
  test-split-vs-account \
  test-transaction-reversal \
  test-transaction-voiding \
  test-business \
  test-address \
  test-customer \
  test-employee \
  test-job \
  test-vendor


test_link_SOURCES = test-link.c
test_link_LDADD = ../libgncmod-engine.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la

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
	utest-Account.c \
	utest-Budget.c \
	utest-Entry.c \
	utest-Invoice.c

test_engine_LDADD = \
	libutest-Split.la \
	libutest-Trans.la \
	$(LDADD)
test_engine_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I${top_srcdir}/${MODULEPATH}/ \
	-DTESTPROG=test_engine \
	${GLIB_CFLAGS}

noinst_LTLIBRARIES = \
	libutest-Split.la \
	libutest-Trans.la

libutest_Split_la_SOURCES = \
	utest-Split.c \
	${top_srcdir}/src/libqof/qof/gnc-numeric.c


libutest_Split_la_LIBADD = $(LDADD)

libutest_Trans_la_SOURCES = \
	utest-Transaction.c

libutest_Trans_la_LIBADD = $(LDADD)

CLEANFILES = .scm-links
DISTCLEANFILES = $(SCM_TESTS)

clean-local:
	rm -f translog.*

distclean-local: clean-local
