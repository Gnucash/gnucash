SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-business-backend-gda.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/backend/gda \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/business/business-core \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${LIBGDA_CFLAGS}

libgncmod_business_backend_gda_la_SOURCES = \
  gncmod-business-backend-gda.c \
  gnc-address-gda.c \
  gnc-bill-term-gda.c \
  gnc-customer-gda.c \
  gnc-employee-gda.c \
  gnc-entry-gda.c \
  gnc-invoice-gda.c \
  gnc-job-gda.c \
  gnc-order-gda.c \
  gnc-owner-gda.c \
  gnc-tax-table-gda.c \
  gnc-vendor-gda.c

noinst_HEADERS = \
  gnc-address-gda.h \
  gnc-bill-term-gda.h \
  gnc-customer-gda.h \
  gnc-employee-gda.h \
  gnc-entry-gda.h \
  gnc-invoice-gda.h \
  gnc-job-gda.h \
  gnc-order-gda.h \
  gnc-owner-gda.h \
  gnc-tax-table-gda.h \
  gnc-vendor-gda.h

libgncmod_business_backend_gda_la_LDFLAGS = -module -avoid-version

libgncmod_business_backend_gda_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/backend/gda/libgnc-backend-gda-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${QOF_LIBS} \
  ${GLIB_LIBS} \
  ${LIBGDA_LIBS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.gda\"
