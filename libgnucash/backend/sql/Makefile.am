include $(top_srcdir)/test-templates/Makefile.decl
SUBDIRS = . test
TEST_SUBDIRS = test

# Now a shared library AND a GModule
lib_LTLIBRARIES = libgnc-backend-sql.la

# "${top_srcdir}/common/debug" below is for the splint header
AM_CPPFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/libgnucash/backend \
  -I${top_srcdir}/common/debug \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/core-utils\
  -I${top_srcdir}/borrowed/libc\
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${WARN_CFLAGS} \
  ${BOOST_CPPFLAGS}

libgnc_backend_sql_la_SOURCES = \
  gnc-account-sql.cpp \
  gnc-address-sql.cpp \
  gnc-bill-term-sql.cpp \
  gnc-book-sql.cpp \
  gnc-budget-sql.cpp \
  gnc-commodity-sql.cpp \
  gnc-customer-sql.cpp \
  gnc-employee-sql.cpp \
  gnc-entry-sql.cpp \
  gnc-invoice-sql.cpp \
  gnc-job-sql.cpp \
  gnc-lots-sql.cpp \
  gnc-order-sql.cpp \
  gnc-owner-sql.cpp \
  gnc-price-sql.cpp \
  gnc-recurrence-sql.cpp \
  gnc-schedxaction-sql.cpp \
  gnc-slots-sql.cpp \
  gnc-tax-table-sql.cpp \
  gnc-transaction-sql.cpp \
  gnc-vendor-sql.cpp \
  gnc-sql-backend.cpp \
  gnc-sql-result.cpp \
  gnc-sql-column-table-entry.cpp \
  gnc-sql-object-backend.cpp \
  escape.cpp

noinst_HEADERS = \
  gnc-account-sql.h \
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
  gnc-price-sql.h \
  gnc-recurrence-sql.h \
  gnc-schedxaction-sql.h \
  gnc-slots-sql.h \
  gnc-tax-table-sql.h \
  gnc-transaction-sql.h \
  gnc-vendor-sql.h \
  gnc-sql-backend.hpp \
  gnc-sql-connection.hpp \
  gnc-sql-result.hpp \
  gnc-sql-column-table-entry.hpp \
  gnc-sql-object-backend.hpp \
  escape.h

libgnc_backend_sql_la_LIBADD = \
   ${GLIB_LIBS} \
   ${GUILE_LIBS} \
   ${top_builddir}/libgnucash/engine/libgncmod-engine.la

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.backend.sql\"

EXTRA_DIST += CMakeLists.txt
