SUBDIRS = . test file
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-business-core.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} 

libgncmod_business_core_la_SOURCES = \
  swig-business-core.c \
  gncBusGuile.c \
  businessmod-core.c \
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

libgncmod_business_core_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}

# business-core-helpers.c

swig-business-core.c: business-core.i ${libgncmod_business_core_la_SOURCES} \
	${noinst_HEADERS}
	swig -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src/engine -o $@ $<

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-core.scm

EXTRA_DIST = \
  ${gncmod_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-business-core-spec.scm
#For executing test cases
SCM_FILE_LINKS += ${gncmod_DATA}
endif

.scm-links: 
	rm -f gnucash g-wrapped
	$(LN_S) -f . gnucash 
	$(LN_S) -f . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links


#BUILT_SOURCES = gw-business-core.scm gw-business-core.c gw-business-core.h
CLEANFILES = $(BUILT_SOURCES) gw-business-core.html \
	g-wrapped gnucash .scm-links ${SCM_FILE_LINKS}
