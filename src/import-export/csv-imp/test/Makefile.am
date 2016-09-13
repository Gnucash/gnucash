# A template Makefile.am for GLib g_test-based test directories.
# Copyright 2011 John Ralls <jralls@ceridwen.us>

include $(top_srcdir)/test-templates/Makefile.decl


#You will only need one of these: It points to the module directory
#after $(top_srcdir) or ${top_builddir}:
MODULEPATH = src/import-export/csv-imp

#The test program. You'll need to add to this if you have more than one module above.

check_PROGRAMS = test-csv-imp

TESTS = ${check_PROGRAMS}

test_csv_impdir = ${top_srcdir}/${MODULEPATH}/test

#Program files for tests go here. It's probably best to have one for
#each file in the parent directory. Include
#test_foo_support.c if you have one and aren't building the
#support library.
test_csv_imp_SOURCES = \
  test-csv-imp.c \
  utest-gnc-csv-model.c \
  utest-gnc-csv-imp-trans.cpp

test_csv_imp_HEADERS =

#The tests might require more libraries, but try to keep them
#as independent as possible.
test_csv_imp_LDADD = \
  ${top_builddir}/${MODULEPATH}/libgncmod-csv-import.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/lib/stf/libgnc-stf.la \
  ${GLIB_LIBS} \
  $(BOOST_LDFLAGS)

test_csv_imp_CFLAGS = \
	-DTESTPROG=test_csv-imp \
	${DEFAULT_INCLUDES} \
	-I$(top_srcdir)/${MODULEPATH}/ \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/lib \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS}

test_csv_imp_CPPFLAGS = \
	-DTESTPROG=test_csv-imp \
	${DEFAULT_INCLUDES} \
	-I$(top_srcdir)/${MODULEPATH}/ \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/lib \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS} \
  $(BOOST_CPPFLAGS)

GNC_TEST_DEPS = \
--library-dir    ${top_builddir}/${MODULEPATH} \
--library-dir    ${top_builddir}/src/import-export \
--library-dir    ${top_builddir}/src/gnome \
--library-dir    ${top_builddir}/src/gnome-utils \
--library-dir    ${top_builddir}/src/gnome-search \
--library-dir    ${top_builddir}/src/register/ledger-core \
--library-dir    ${top_builddir}/src/register/register-core \
--library-dir    ${top_builddir}/src/register/register-gnome \
--library-dir    ${top_builddir}/src/report/report-system \
--library-dir    ${top_builddir}/src/report/report-gnome \
--library-dir    ${top_builddir}/src/html \
--library-dir    ${top_builddir}/src/app-utils \
--library-dir    ${top_builddir}/src/backend/xml \
--library-dir    ${top_builddir}/src/engine \
--library-dir    ${top_builddir}/src/core-utils \
--library-dir    ${top_builddir}/src/gnc-module \
--library-dir    ${top_builddir}/src/libqof/qof \
--library-dir    ${top_builddir}/lib/stf

TESTS_ENVIRONMENT = \
  SRCDIR=${srcdir} \
  G_DEBUG= \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

test_tokenizer_SOURCES = \
    test-tokenizer.cpp
test_tokenizer_LDADD = \
  -L${top_builddir}/${MODULEPATH} \
  -L${top_builddir}/src/import-export \
  -L${top_builddir}/src/gnome \
  -L${top_builddir}/src/gnome-utils \
  -L${top_builddir}/src/gnome-search \
  -L${top_builddir}/src/register/ledger-core \
  -L${top_builddir}/src/register/register-core \
  -L${top_builddir}/src/register/register-gnome \
  -L${top_builddir}/src/report/report-system \
  -L${top_builddir}/src/report/report-gnome \
  -L${top_builddir}/src/html \
  -L${top_builddir}/src/app-utils \
  -L${top_builddir}/src/backend/xml \
  -L${top_builddir}/src/engine \
  -L${top_builddir}/src/core-utils \
  -L${top_builddir}/src/gnc-module \
  -L${top_builddir}/src/libqof/qof \
  -L${top_builddir}/lib/stf \
  ${top_builddir}/${MODULEPATH}/libgncmod-csv-import.la \
  ${top_builddir}/src/import-export/libgncmod-generic-import.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/lib/stf/libgnc-stf.la \
  ${GLIB_LIBS} \
  $(GTEST_LIBS) \
  $(BOOST_LDFLAGS)

if !GOOGLE_TEST_LIBS
nodist_test_tokenizer_SOURCES = \
        ${GTEST_SRC}/src/gtest_main.cc
endif

test_tokenizer_CPPFLAGS = \
  -I$(GTEST_HEADERS) \
  -I$(top_srcdir)/$(MODULEPATH) \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/import-export \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/lib \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS} \
  $(BOOST_CPPFLAGS)

check_PROGRAMS += test-tokenizer

EXTRA_DIST= \
  sample1.csv


AM_CPPFLAGS = -DG_LOG_DOMAIN=\"gnc.import.csv\"
