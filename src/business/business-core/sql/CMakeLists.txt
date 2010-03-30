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
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/business/business-core)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/backend/sql)

SET (libgnc_business_backend_sql_SOURCES
  gncmod-business-backend-sql.c
  gnc-address-sql.c
  gnc-bill-term-sql.c
  gnc-customer-sql.c
  gnc-employee-sql.c
  gnc-entry-sql.c
  gnc-invoice-sql.c
  gnc-job-sql.c
  gnc-order-sql.c
  gnc-owner-sql.c
  gnc-tax-table-sql.c
  gnc-vendor-sql.c
)
SET (libgnc_business_backend_sql_HEADERS
  gnc-address-sql.h
  gnc-bill-term-sql.h
  gnc-customer-sql.h
  gnc-employee-sql.h
  gnc-entry-sql.h
  gnc-invoice-sql.h
  gnc-job-sql.h
  gnc-order-sql.h
  gnc-owner-sql.h
  gnc-tax-table-sql.h
  gnc-vendor-sql.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_business_backend_sql_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-business-backend-sql
  ${libgnc_business_backend_sql_SOURCES}
  ${libgnc_business_backend_sql_HEADERS}
  )
