# CMakeLists.txt for src/backend/dbi

ADD_SUBDIRECTORY(test)

SET (backend_sql_SOURCES
  gnc-backend-sql.cpp
  gnc-account-sql.cpp
  gnc-address-sql.cpp
  gnc-bill-term-sql.cpp
  gnc-book-sql.cpp
  gnc-budget-sql.cpp
  gnc-commodity-sql.cpp
  gnc-customer-sql.cpp
  gnc-employee-sql.cpp
  gnc-entry-sql.cpp
  gnc-invoice-sql.cpp
  gnc-job-sql.cpp
  gnc-lots-sql.cpp
  gnc-order-sql.cpp
  gnc-owner-sql.cpp
  gnc-price-sql.cpp
  gnc-recurrence-sql.cpp
  gnc-schedxaction-sql.cpp
  gnc-slots-sql.cpp
  gnc-tax-table-sql.cpp
  gnc-transaction-sql.cpp
  gnc-vendor-sql.cpp
  escape.cpp
)
SET (backend_sql_noinst_HEADERS
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
SET_SOURCE_FILES_PROPERTIES (${gnc_backend_sql_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-sql
  ${backend_sql_SOURCES}
  ${backend_sql_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gnc-backend-sql gncmod-engine gnc-qof)

TARGET_COMPILE_DEFINITIONS (gnc-backend-sql PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.sql\")

TARGET_INCLUDE_DIRECTORIES(gnc-backend-sql PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

INSTALL(TARGETS gnc-backend-sql
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin)

# No headers to install
