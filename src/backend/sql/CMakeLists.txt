# CMakeLists.txt for src/backend/dbi

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.backend.sql\")

INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)

SET (libgnc_backend_sql_SOURCES
  gnc-backend-sql.c
  gnc-account-sql.c
  gnc-address-sql.c
  gnc-bill-term-sql.c
  gnc-book-sql.c
  gnc-budget-sql.c
  gnc-commodity-sql.c
  gnc-customer-sql.c
  gnc-employee-sql.c
  gnc-entry-sql.c
  gnc-invoice-sql.c
  gnc-job-sql.c
  gnc-lots-sql.c
  gnc-order-sql.c
  gnc-owner-sql.c
  gnc-price-sql.c
  gnc-recurrence-sql.c
  gnc-schedxaction-sql.c
  gnc-slots-sql.c
  gnc-tax-table-sql.c
  gnc-transaction-sql.c
  gnc-vendor-sql.c
  escape.c
)
SET (libgnc_backend_sql_HEADERS
  gnc-account-sql.h
  gnc-address-sql.h
  gnc-backend-sql.h
  gnc-bill-term-sql.h
  gnc-book-sql.h
  gnc-budget-sql.h
  gnc-commodity-sql.h
  gnc-customer-sql.h
  gnc-employee-sql.h
  gnc-entry-sql.h
  gnc-invoice-sql.h
  gnc-job-sql.h
  gnc-lots-sql.h
  gnc-order-sql.h
  gnc-owner-sql.h
  gnc-price-sql.h
  gnc-recurrence-sql.h
  gnc-schedxaction-sql.h
  gnc-slots-sql.h
  gnc-tax-table-sql.h
  gnc-transaction-sql.h
  gnc-vendor-sql.h
  escape.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_backend_sql_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-sql
  ${libgnc_backend_sql_SOURCES}
  ${libgnc_backend_sql_HEADERS}
  )
