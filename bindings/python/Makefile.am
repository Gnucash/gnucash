SUBDIRS = . tests

SWIG_FILES = gnucash_core.i timespec.i

pkgpyexec_DATA = \
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
  -I$(top_srcdir)/common  \
  -I$(top_srcdir)/libgnucash/engine \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module

# Suppress all warnings for now, but we really only need to -Wno-implicit
AM_CFLAGS = -w

_gnucash_core_c_la_LDFLAGS = -avoid-version -module

_gnucash_core_c_la_LIBADD = \
  ${GLIB_LIBS} \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la
if BUILDING_FROM_VCS
_gnucash_core_c_includes= \
	${top_builddir}/config.h \
	${top_srcdir}/libgnucash/engine/qofsession.h \
	${top_srcdir}/libgnucash/engine/qofbook.h \
	${top_srcdir}/libgnucash/engine/qofbackend.h \
	${top_srcdir}/libgnucash/engine/qoflog.h \
	${top_srcdir}/libgnucash/engine/qofutil.h \
	${top_srcdir}/libgnucash/engine/qofid.h \
	${top_srcdir}/libgnucash/engine/guid.h \
	${top_srcdir}/libgnucash/gnc-module/gnc-module.h \
	${top_srcdir}/libgnucash/engine/gnc-engine.h \
	${top_srcdir}/libgnucash/engine/Transaction.h \
	${top_srcdir}/libgnucash/engine/Split.h \
	${top_srcdir}/libgnucash/engine/Account.h \
	${top_srcdir}/libgnucash/engine/gnc-commodity.h \
	${top_srcdir}/libgnucash/engine/gnc-lot.h \
	${top_srcdir}/libgnucash/engine/gnc-numeric.h \
	${top_srcdir}/libgnucash/engine/gncCustomer.h \
	${top_srcdir}/libgnucash/engine/gncEmployee.h \
	${top_srcdir}/libgnucash/engine/gncVendor.h \
	${top_srcdir}/libgnucash/engine/gncAddress.h \
	${top_srcdir}/libgnucash/engine/gncBillTerm.h \
	${top_srcdir}/libgnucash/engine/gncOwner.h \
	${top_srcdir}/libgnucash/engine/gncInvoice.h \
	${top_srcdir}/libgnucash/engine/gncJob.h \
	${top_srcdir}/libgnucash/engine/gncEntry.h \
	${top_srcdir}/libgnucash/engine/gncTaxTable.h \
	${top_srcdir}/libgnucash/engine/gncIDSearch.h \
	${top_srcdir}/libgnucash/engine/gnc-pricedb.h \
	${top_srcdir}/libgnucash/app-utils/gnc-prefs-utils.h


gnucash_core.c: $(SWIG_FILES) ${top_srcdir}/common/base-typemaps.i ${top_srcdir}/libgnucash/engine/engine-common.i $(_gnucash_core_c_includes)
	$(SWIG) -python -Wall -Werror \
	-I$(top_srcdir)/common -I$(top_srcdir)/libgnucash/engine \
	-I$(top_srcdir)/libgnucash/app-utils -o $@ $<

gnucash_core_c.py: gnucash_core.c $(SWIG_FILES)
endif

EXTRA_DIST = \
  $(pkgpyexec_DATA) \
  $(SWIG_FILES) \
  example_scripts/Invoice.tex \
  example_scripts/latex_invoices.py \
  example_scripts/simple_book.py \
  example_scripts/simple_session.py \
  example_scripts/simple_test.py \
  example_scripts/simple_business_create.py \
  example_scripts/simple_invoice_insert.py \
  example_scripts/simple_sqlite_create.py \
  example_scripts/change_tax_code.py \
  example_scripts/account_analysis.py \
  example_scripts/new_book_with_opening_balances.py \
  example_scripts/test_imbalance_transaction.py \
  example_scripts/rest-api/gnucash_rest.py \
  example_scripts/rest-api/gnucash_simple.py \
  example_scripts/rest-api/README \
  example_scripts/CMakeLists.txt \
  CMakeLists.txt

MAINTAINERCLEANFILES = gnucash_core.c


check_PROGRAMS = sqlite3test
sqlite3test_SOURCES = sqlite3test.c
sqlite3test_LDADD = ${_gnucash_core_c_la_LIBADD}
sqlite3test_CPPFLAGS = ${_gnucash_core_c_la_CPPFLAGS}

PYTHON_GNUCASH_LINK_FILES = \
  $(pkgpyexec_DATA)


if BUILDING_FROM_VCS
  PYTHON_LINK_FILES = $(filter-out gnucash_core_c.py,${PYTHON_GNUCASH_LINK_FILES})
else
  PYTHON_LINK_FILES = ${PYTHON_GNUCASH_LINK_FILES}
endif

.py-links:$(PYTHON_GNUCASH_LINK_FILES)
	$(RM) -rf gnucash
	mkdir -p gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${PYTHON_LINK_FILES} ; do \
	    $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for file in $(PYTHON_GNUCASH_LINK_FILES) ; do \
	  $(LN_S) -f ../$$file .; \
	  done )


if ! OS_WIN32
	touch .py-links
endif

noinst_DATA = .py-links

CLEANFILES = $(BUILT_SOURCES) .py-links gnucash_core.c.py

clean-local:
	rm -rf gnucash

uninstall-local:
	rm -rf ${pkgpyexecdir}
