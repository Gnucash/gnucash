SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-business-backend-xml.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/backend/xml \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/business/business-core \
  ${LIBXML2_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}

libgncmod_business_backend_xml_la_SOURCES = \
  gncmod-business-backend-xml.c \
  gnc-address-xml-v2.c \
  gnc-bill-term-xml-v2.c \
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
  gnc-bill-term-xml-v2.h \
  gnc-customer-xml-v2.h \
  gnc-employee-xml-v2.h \
  gnc-entry-xml-v2.h \
  gnc-invoice-xml-v2.h \
  gnc-job-xml-v2.h \
  gnc-order-xml-v2.h \
  gnc-owner-xml-v2.h \
  gnc-tax-table-xml-v2.h \
  gnc-vendor-xml-v2.h \
  xml-helpers.h

libgncmod_business_backend_xml_la_LDFLAGS = -module -avoid-version

libgncmod_business_backend_xml_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${LIBXML2_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}
