# Note the test subdirectory is added in src/Makefile.am rather
# than here. This is because we need it to build after src/test-core
# but that directory requires src/engine to be built already...
SUBDIRS = . test-core

pkglib_LTLIBRARIES = libgncmod-engine.la

AM_CPPFLAGS = \
	-I${top_srcdir}/borrowed/libc \
	-I${top_srcdir}/libgnucash/core-utils \
	-I${top_srcdir}/common \
	-I${top_builddir}/common \
	-I${top_srcdir}/libgnucash/gnc-module \
	${GLIB_CFLAGS} \
	${GUILE_CFLAGS} \
	$(BOOST_CPPFLAGS)


libgncmod_engine_la_SOURCES = \
  Account.cpp \
  Recurrence.c \
  Query.c \
  SchedXaction.c \
  SX-book.c \
  SX-ttinfo.c \
  Scrub.c \
  Scrub2.c \
  Scrub3.c \
  ScrubBusiness.c \
  Split.c \
  TransLog.c \
  Transaction.c \
  cap-gains.c \
  cashobjects.c \
  gnc-aqbanking-templates.cpp \
  gnc-budget.c \
  gnc-commodity.c \
  gnc-date.cpp \
  gnc-datetime.cpp \
  gnc-engine.c \
  gnc-event.c \
  gnc-features.c \
  gnc-hooks.c \
  gnc-int128.cpp \
  gnc-lot.c \
  gnc-numeric.cpp \
  gnc-pricedb.c \
  gnc-rational.cpp \
  gnc-session.c \
  gnc-timezone.cpp \
  gnc-uri-utils.c \
  gncmod-engine.c \
  guid.cpp \
  swig-engine.c \
  kvp-frame.cpp \
  kvp-scm.cpp \
  kvp-value.cpp \
  engine-helpers.c \
  glib-helpers.c  \
  policy.c \
  gncBusGuile.c \
  gncBusiness.c \
  gncAddress.c \
  gncBillTerm.c \
  gncCustomer.c \
  gncEmployee.c \
  gncEntry.c \
  gncInvoice.c \
  gncJob.c \
  gncOrder.c \
  gncOwner.c \
  gncTaxTable.c \
  gncIDSearch.c \
  gncVendor.c \
  qof-backend.cpp \
  qofbook.cpp \
  qofchoice.cpp \
  qofclass.cpp \
  qofevent.cpp \
  qofid.cpp \
  qofinstance.cpp \
  qoflog.cpp \
  qofobject.cpp \
  qofquery.cpp \
  qofquerycore.cpp \
  qofsession.cpp \
  qof-string-cache.cpp \
  qofutil.cpp

if OS_WIN32
libgncmod_engine_la_SOURCES += qof-win32.cpp
else
WIN32_EXTRA_DIST = qof-win32.cpp
endif

EXTRA_libgncmod_engine_la_SOURCES = iso-4217-currencies.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  Account.h \
  FreqSpec.h \
  Recurrence.h \
  SchedXaction.h \
  SX-book.h \
  SX-ttinfo.h \
  Query.h \
  Scrub.h \
  Scrub2.h \
  Scrub3.h \
  ScrubBusiness.h \
  Split.h \
  TransLog.h \
  Transaction.h \
  cap-gains.h \
  cashobjects.h \
  engine-helpers.h \
  engine-helpers-guile.h \
  glib-helpers.h \
  gnc-aqbanking-templates.h \
  gnc-budget.h \
  gnc-commodity.h \
  gnc-date.h \
  gnc-datetime.hpp \
  gnc-engine.h \
  gnc-event.h \
  gnc-features.h \
  gnc-hooks.h \
  gnc-numeric.h \
  gnc-numeric.hpp \
  gnc-pricedb.h \
  gnc-rational.hpp \
  gnc-rational-rounding.hpp \
  gnc-session.h \
  gnc-timezone.hpp \
  gnc-uri-utils.h \
  gncAddress.h \
  gncAddressP.h \
  gncBillTerm.h \
  gncBillTermP.h \
  gncBusiness.h \
  gncBusGuile.h \
  gncCustomer.h \
  gncCustomerP.h \
  gncEmployee.h \
  gncEmployeeP.h \
  gncEntry.h \
  gncEntryP.h \
  gncInvoice.h \
  gncInvoiceP.h \
  gncJob.h \
  gncJobP.h \
  gncOrder.h \
  gncOrderP.h \
  gncOwner.h \
  gncOwnerP.h \
  gncTaxTable.h \
  gncTaxTableP.h \
  gncIDSearch.h \
  gncVendor.h \
  gncVendorP.h \
  guid.h \
  guid.hpp \
  kvp-frame.hpp \
  kvp-scm.h \
  kvp-value.hpp \
  policy.h \
  qof.h \
  qof-backend.hpp \
  qofbackend.h \
  qofbook.h \
  qofbookslots.h \
  qofchoice.h \
  qofclass.h \
  qofevent.h \
  qofid-p.h \
  qofid.h \
  qofinstance-p.h \
  qofinstance.h \
  qoflog.h \
  qofobject.h \
  qofquery.h \
  qofquerycore.h \
  qofsession.h \
  qofsession.hpp \
  qofutil.h \
  qof-gobject.h \
  qof-string-cache.h

noinst_HEADERS = \
  Account.hpp \
  ScrubP.h \
  SplitP.h \
  SX-book.h \
  SX-ttinfo.h \
  TransactionP.h \
  gnc-backend-prov.hpp \
  gnc-date-p.h \
  gnc-hooks-scm.h \
  gnc-int128.hpp \
  gnc-lot.h \
  gnc-lot-p.h \
  gnc-pricedb-p.h \
  policy-p.h  \
  qofbook-p.h \
  qofclass-p.h \
  qofevent-p.h \
  qofobject-p.h \
  qofquery-p.h \
  qofquerycore-p.h

libgncmod_engine_la_LDFLAGS = -avoid-version
if PLATFORM_WIN32
libgncmod_engine_la_LDFLAGS += -Wl,-export-all-symbols
endif
if CLANG
libgncmod_engine_la_CFLAGS = ${AM_CFLAGS} -Wno-invalid-source-encoding
else
libgncmod_engine_la_CFLAGS = ${AM_CFLAGS}
endif
libgncmod_engine_la_LIBADD = \
  ../gnc-module/libgnc-module.la \
  ../core-utils/libgnc-core-utils.la \
  ${GUILE_LIBS} \
  ${REGEX_LIBS} \
  ${GLIB_LIBS} \
  ${BOOST_LDFLAGS} -lboost_regex \
  ${top_builddir}/borrowed/libc/libc-missing.la

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash
gncscmmod_DATA = engine.scm \
  business-core.scm

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscm_DATA = \
  commodity-table.scm \
  engine-interface.scm \
  engine-utilities.scm \
  gnc-numeric.scm

EXTRA_DIST = \
  README.query-api \
  design.txt \
  extensions.txt \
  iso-4217-currencies.xml \
  iso-currencies-to-c.xsl \
  kvp_doc.txt \
  SX-book-p.h \
  engine.i \
  engine-common.i \
  business-core.i \
  ${gncscmmod_DATA} \
  ${gncscm_DATA} \
  CMakeLists.txt \
  ${WIN32_EXTRA_DIST}

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/core-utils

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

noinst_DATA = .scm-links

if BUILDING_FROM_VCS

swig-engine.c: engine.i $(top_srcdir)/common/base-typemaps.i \
               $(gncinclude_HEADERS) $(noinst_HEADERS)
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/common -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/common/swig-utf8.patch; \
	fi
endif
endif
endif


iso-4217-currencies.c: iso-4217-currencies.xml iso-currencies-to-c.xsl
	$(XSLTPROC) -o $@ $(srcdir)/iso-currencies-to-c.xsl $<

BUILT_SOURCES = iso-4217-currencies.c

CLEANFILES = \
	$(BUILT_SOURCES) \
	.scm-links \
	${SCM_FILE_LINKS} \
	${gncscmmodcache_DATA} \
	${gncscmcache_DATA}

MAINTAINERCLEANFILES = swig-engine.c

clean-local:
	rm -rf gnucash

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.engine\"
