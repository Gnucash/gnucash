SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-business-backend-file.la

AM_CFLAGS = \
  -I.. -I../../.. \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/backend/file \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS}

libgncmod_business_backend_file_la_SOURCES = \
  gncmod-business-backend-file.c \
  gnc-address-xml-v2.c \
  gnc-customer-xml-v2.c \
  gnc-employee-xml-v2.c \
  gnc-entry-xml-v2.c \
  gnc-invoice-xml-v2.c \
  gnc-job-xml-v2.c \
  gnc-order-xml-v2.c \
  gnc-owner-xml-v2.c \
  gnc-tax-table-xml-v2.c \
  gnc-vendor-xml-v2.c

noinst_HEADERS = \
  gnc-address-xml-v2.h \
  gnc-customer-xml-v2.h \
  gnc-employee-xml-v2.h \
  gnc-entry-xml-v2.h \
  gnc-invoice-xml-v2.h \
  gnc-job-xml-v2.h \
  gnc-order-xml-v2.h \
  gnc-owner-xml-v2.h \
  gnc-tax-table-xml-v2.h \
  gnc-vendor-xml-v2.h

libgncmod_business_backend_file_la_LDFLAGS = -module
libgncmod_business_backend_file_la_LIBADD = \
   ${GLIB_LIBS} ${GNOME_XML_LIB} \
   ${top_builddir}/src/business/business-core/libgncmod-business-core.la
