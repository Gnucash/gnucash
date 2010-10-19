SUBDIRS = . test

# Now a shared library AND a GModule
lib_LTLIBRARIES = libgnc-backend-sql.la

# "${top_srcdir}/src/debug" below is for the splint header
AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/debug \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils\
  -I${top_srcdir}/lib/libc\
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS} \
  ${GCONF_CFLAGS} \
  ${WARN_CFLAGS}

libgnc_backend_sql_la_SOURCES = \
  gnc-backend-sql.c \
  gnc-account-sql.c \
  gnc-address-sql.c \
  gnc-bill-term-sql.c \
  gnc-book-sql.c \
  gnc-budget-sql.c \
  gnc-commodity-sql.c \
  gnc-customer-sql.c \
  gnc-employee-sql.c \
  gnc-entry-sql.c \
  gnc-invoice-sql.c \
  gnc-job-sql.c \
  gnc-lots-sql.c \
  gnc-order-sql.c \
  gnc-owner-sql.c \
  gnc-price-sql.c \
  gnc-recurrence-sql.c \
  gnc-schedxaction-sql.c \
  gnc-slots-sql.c \
  gnc-tax-table-sql.c \
  gnc-transaction-sql.c \
  gnc-vendor-sql.c \
  escape.c

noinst_HEADERS = \
  gnc-account-sql.h \
  gnc-address-sql.h \
  gnc-backend-sql.h \
  gnc-bill-term-sql.h \
  gnc-book-sql.h \
  gnc-budget-sql.h \
  gnc-commodity-sql.h \
  gnc-customer-sql.h \
  gnc-employee-sql.h \
  gnc-entry-sql.h \
  gnc-invoice-sql.h \
  gnc-job-sql.h \
  gnc-lots-sql.h \
  gnc-order-sql.h \
  gnc-owner-sql.h \
  gnc-price-sql.h \
  gnc-recurrence-sql.h \
  gnc-schedxaction-sql.h \
  gnc-slots-sql.h \
  gnc-tax-table-sql.h \
  gnc-transaction-sql.h \
  gnc-vendor-sql.h \
  escape.h

libgnc_backend_sql_la_LIBADD = \
   ${GLIB_LIBS} \
   ${GUILE_LIBS} \
   ${GCONF_LIBS} \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   ${top_builddir}/src/libqof/qof/libgnc-qof.la

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.sql\"
