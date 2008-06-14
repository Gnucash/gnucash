SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-business-backend-sql.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/backend/sql \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/business/business-core \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${LIBGDA_CFLAGS}

libgncmod_business_backend_sql_la_SOURCES = \
  gncmod-business-backend-sql.c \
  gnc-address-sql.c \
  gnc-bill-term-sql.c \
  gnc-customer-sql.c \
  gnc-employee-sql.c \
  gnc-entry-sql.c \
  gnc-invoice-sql.c \
  gnc-job-sql.c \
  gnc-order-sql.c \
  gnc-owner-sql.c \
  gnc-tax-table-sql.c \
  gnc-vendor-sql.c

noinst_HEADERS = \
  gnc-address-sql.h \
  gnc-bill-term-sql.h \
  gnc-customer-sql.h \
  gnc-employee-sql.h \
  gnc-entry-sql.h \
  gnc-invoice-sql.h \
  gnc-job-sql.h \
  gnc-order-sql.h \
  gnc-owner-sql.h \
  gnc-tax-table-sql.h \
  gnc-vendor-sql.h

libgncmod_business_backend_sql_la_LDFLAGS = -module -avoid-version

libgncmod_business_backend_sql_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/backend/sql/libgncmod-backend-sql.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${QOF_LIBS} \
  ${GLIB_LIBS} \
  ${LIBGDA_LIBS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.sql\"
