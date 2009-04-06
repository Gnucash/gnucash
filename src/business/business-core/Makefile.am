SUBDIRS = . sql test xml

pkglib_LTLIBRARIES = libgncmod-business-core.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

libgncmod_business_core_la_SOURCES = \
  swig-business-core.c \
  gncBusGuile.c \
  gncmod-business-core.c \
  gncAddress.c \
  gncBillTerm.c \
  gncBusiness.c \
  gncCustomer.c \
  gncEmployee.c \
  gncEntry.c \
  gncInvoice.c \
  gncJob.c \
  gncOrder.c \
  gncOwner.c \
  gncTaxTable.c \
  gncVendor.c

noinst_HEADERS = \
  gncAddress.h \
  gncAddressP.h \
  gncBillTerm.h \
  gncBillTermP.h \
  gncBusiness.h \
  gncBusinessP.h \
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
  gncVendor.h \
  gncVendorP.h

libgncmod_business_core_la_LDFLAGS = -avoid-version

libgncmod_business_core_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${QOF_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}

# business-core-helpers.c

if BUILDING_FROM_SVN
swig-business-core.c: business-core.i ${noinst_HEADERS} \
                      ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-core.scm

EXTRA_DIST = business-core.i ${gncmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncmod_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

noinst_DATA = .scm-links
CLEANFILES = gnucash .scm-links ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = swig-business-core.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.business.core\"
