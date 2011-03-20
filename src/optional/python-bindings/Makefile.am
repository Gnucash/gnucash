SUBDIRS = . tests

SWIG_FILES = gnucash_core.i timespec.i

pkgpython_PYTHON = \
  __init__.py \
  function_class.py \
  gnucash_core.py \
  gnucash_core_c.py \
  gnucash_business.py

pkgpyexec_LTLIBRARIES = _gnucash_core_c.la

_gnucash_core_c_la_SOURCES = \
  gnucash_core.c

_gnucash_core_c_la_CPPFLAGS = \
  $(PYTHON_CPPFLAGS) \
  $(GLIB_CFLAGS) \
  $(GNOME_CFLAGS) \
  -I${top_srcdir}/src/libqof/qof \
  -I$(top_srcdir)/src  \
  -I$(top_srcdir)/src/engine \
  -I$(top_srcdir)/src/business/business-core \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome \
	-I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module

# Suppress all warnings for now, but we really only need to -Wno-implicit
AM_CFLAGS = -w

_gnucash_core_c_la_LDFLAGS = -avoid-version -module

_gnucash_core_c_la_LIBADD = \
  ${GLIB_LIBS} \
  ${GNOME_LIBS} \
  ${GLADE_LIBS} \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la
if BUILDING_FROM_SVN
_gnucash_core_c_includes= \
	${top_builddir}/config.h \
	${top_srcdir}/src/libqof/qof/qofsession.h \
	${top_srcdir}/src/libqof/qof/qofbook.h \
	${top_srcdir}/src/libqof/qof/qofbackend.h \
	${top_srcdir}/src/libqof/qof/qoflog.h \
	${top_srcdir}/src/libqof/qof/qofutil.h \
	${top_srcdir}/src/libqof/qof/qofid.h \
	${top_srcdir}/src/libqof/qof/guid.h \
	${top_srcdir}/src/gnc-module/gnc-module.h \
	${top_srcdir}/src/engine/gnc-engine.h \
	${top_srcdir}/src/engine/Transaction.h \
	${top_srcdir}/src/gnc/Transaction.hpp \
	${top_srcdir}/src/engine/Split.h \
	${top_srcdir}/src/engine/Account.h \
	${top_srcdir}/src/engine/gnc-commodity.h \
	${top_srcdir}/src/engine/gnc-lot.h \
	${top_srcdir}/src/libqof/qof/gnc-numeric.h \
	${top_srcdir}/src/engine/gncCustomer.h \
	${top_srcdir}/src/engine/gncEmployee.h \
	${top_srcdir}/src/engine/gncVendor.h \
	${top_srcdir}/src/engine/gncAddress.h \
	${top_srcdir}/src/engine/gncBillTerm.h \
	${top_srcdir}/src/engine/gncOwner.h \
	${top_srcdir}/src/engine/gncInvoice.h \
	${top_srcdir}/src/engine/gncJob.h \
	${top_srcdir}/src/engine/gncEntry.h \
	${top_srcdir}/src/engine/gncTaxTable.h \
	${top_srcdir}/src/engine/gncIDSearch.h \
	${top_srcdir}/src/engine/gnc-pricedb.h


gnucash_core.c: $(SWIG_FILES) ${top_srcdir}/src/base-typemaps.i ${top_srcdir}/src/engine/engine-common.i $(_gnucash_core_c_includes)
	swig -python -Wall -Werror \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/engine \
	-I$(top_srcdir)/src/business/business-core \
	-I${top_srcdir}/src/libqof/qof \
	-o $@ $<

gnucash_core_c.py: gnucash_core.c $(SWIG_FILES)
endif

EXTRA_DIST = \
  $(SWIG_FILES) \
  example_scripts/simple_book.py \
  example_scripts/simple_session.py \
  example_scripts/simple_test.py \
  example_scripts/simple_business_create.py \
  example_scripts/simple_invoice_insert.py \
  example_scripts/simple_sqlite_create.py \
  example_scripts/change_tax_code.py \
  example_scripts/account_analysis.py \
  example_scripts/new_book_with_opening_balances.py \
  example_scripts/test_imbalance_transaction.py

MAINTAINERCLEANFILES = gnucash_core.c


check_PROGRAMS = sqlite3test
sqlite3test_SOURCES = sqlite3test.c
sqlite3test_LDADD = ${_gnucash_core_c_la_LIBADD}
sqlite3test_CPPFLAGS = ${_gnucash_core_c_la_CPPFLAGS}

