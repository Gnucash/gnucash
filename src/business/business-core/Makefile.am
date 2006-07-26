SUBDIRS = . test file
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-business-core.la libgw-business-core.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} 

libgncmod_business_core_la_SOURCES = \
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

libgw_business_core_la_SOURCES = \
  gncBusGuile.c

nodist_libgw_business_core_la_SOURCES = \
  gw-business-core.c

libgw_business_core_la_LDFLAGS = ${G_WRAP_LINK_ARGS}
libgw_business_core_la_LIBADD  = \
  libgncmod-business-core.la \
  ${top_builddir}/src/engine/libgw-engine.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-core.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = \
  gw-business-core-spec.scm
nodist_gwmod_DATA = \
  gw-business-core.scm

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

gw-business-core.scm gw-business-core.c gw-business-core.h: \
 .scm-links gw-business-core-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (set! %load-path (cons \"../../engine\" %load-path)) \
	 (primitive-load \"./gw-business-core-spec.scm\") \
	 (gw:generate-wrapset \"gw-business-core\")"

BUILT_SOURCES = gw-business-core.scm gw-business-core.c gw-business-core.h
CLEANFILES = $(BUILT_SOURCES) gw-business-core.html \
	g-wrapped gnucash .scm-links ${SCM_FILE_LINKS}
