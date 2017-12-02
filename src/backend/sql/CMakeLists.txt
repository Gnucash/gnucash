# CMakeLists.txt for src/backend/dbi

ADD_SUBDIRECTORY(test)

SET (backend_sql_SOURCES
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

SET_LOCAL_DIST(backend_sql_DIST_local ${backend_sql_SOURCES} ${backend_sql_noinst_HEADERS} CMakeLists.txt Makefile.am)
SET(backend_sql_DIST ${backend_sql_DIST_local} ${test_backend_sql_DIST} PARENT_SCOPE)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${gnc_backend_sql_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

IF (WITH_SQL)

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
ENDIF(WITH_SQL)

# No headers to install
