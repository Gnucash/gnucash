SUBDIRS = . test

pkglib_LTLIBRARIES = libgw-business-core.la libgncmod-business-core.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} 

libgncmod_business_core_la_SOURCES = \
  businessmod-core.c \
  gncAddress.c \
  gncBusiness.c \
  gncCustomer.c \
  gncEmployee.c \
  gncEntry.c \
  gncInvoice.c \
  gncJob.c \
  gncOrder.c \
  gncOwner.c \
  gncVendor.c

noinst_HEADERS = \
  gncAddress.h \
  gncBusiness.h \
  gncBusinessP.h \
  gncCustomer.h \
  gncCustomerP.h \
  gncEmployee.h \
  gncEmployeeP.h \
  gncEntry.h \
  gncEntryP.h \
  gncInvoice.h \
  gncJob.h \
  gncJobP.h \
  gncOrder.h \
  gncOwner.h \
  gncVendor.h \
  gncVendorP.h

libgncmod_business_core_la_LDFLAGS = -module

libgncmod_business_core_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}

# business-core-helpers.c

libgw_business_core_la_SOURCES = gw-business-core.c 
libgw_business_core_la_LDFLAGS = -module ${G_WRAP_LINK_ARGS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-core.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = \
  gw-business-core.scm \
  gw-business-core-spec.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncmod_DATA} \
  ${gwmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gncmod-business-core.c: gw-business-core.h

gw-business-core.scm gw-business-core.c gw-business-core.h: .scm-links gw-business-core-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (set! %load-path (cons \"../../engine\" %load-path)) \
	 (primitive-load \"./gw-business-core-spec.scm\") \
	 (gw:generate-wrapset \"gw-business-core\")"

BUILT_SOURCES = gw-business-core.scm gw-business-core.c gw-business-core.h
CLEANFILES = gw-business-core.scm gw-business-core.c gw-business-core.h gw-business-core.html
