# CMakeLists.txt for libgnucash/backend/dbi

add_subdirectory(test)

set (backend_sql_SOURCES
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
  gnc-sql-backend.cpp
  gnc-sql-result.cpp
  gnc-sql-column-table-entry.cpp
  gnc-sql-object-backend.cpp
  escape.cpp
)
set (backend_sql_noinst_HEADERS
  gnc-account-sql.h
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
  gnc-price-sql.h
  gnc-recurrence-sql.h
  gnc-schedxaction-sql.h
  gnc-slots-sql.h
  gnc-tax-table-sql.h
  gnc-transaction-sql.h
  gnc-vendor-sql.h
  gnc-sql-backend.hpp
  gnc-sql-connection.hpp
  gnc-sql-result.hpp
  gnc-sql-column-table-entry.hpp
  gnc-sql-object-backend.hpp
  escape.h
)

set_local_dist(backend_sql_DIST_local ${backend_sql_SOURCES} ${backend_sql_noinst_HEADERS} CMakeLists.txt )
set(backend_sql_DIST ${backend_sql_DIST_local} ${test_backend_sql_DIST} PARENT_SCOPE)

# Add dependency on config.h
set_source_files_properties (${gnc_backend_sql_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})
if(WITH_SQL)
  add_library (gnc-backend-sql
    ${backend_sql_SOURCES}
    ${backend_sql_noinst_HEADERS}
    )

  target_link_libraries(gnc-backend-sql gncmod-engine)

  target_compile_definitions (gnc-backend-sql PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.sql\")

  target_include_directories(gnc-backend-sql PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

  install(TARGETS gnc-backend-sql
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

  # No headers to install
endif()
