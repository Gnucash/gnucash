# CMakeLists.txt for libgnucash/backend/xml

add_subdirectory(test)

set (backend_xml_utils_noinst_HEADERS
  gnc-backend-xml.h
  gnc-xml.h
  gnc-address-xml-v2.h
  gnc-bill-term-xml-v2.h
  gnc-customer-xml-v2.h
  gnc-employee-xml-v2.h
  gnc-entry-xml-v2.h
  gnc-invoice-xml-v2.h
  gnc-job-xml-v2.h
  gnc-order-xml-v2.h
  gnc-owner-xml-v2.h
  gnc-tax-table-xml-v2.h
  gnc-vendor-xml-v2.h
  gnc-xml-backend.hpp
  gnc-xml-helper.h
  io-example-account.h
  io-gncxml-gen.h
  io-gncxml-v2.h
  io-gncxml.h
  io-utils.h
  sixtp-dom-generators.h
  sixtp-dom-parsers.h
  sixtp-parsers.h
  sixtp-stack.h
  sixtp-utils.h
  sixtp.h
  xml-helpers.h
)

set (backend_xml_utils_SOURCES
  gnc-account-xml-v2.cpp
  gnc-address-xml-v2.cpp
  gnc-bill-term-xml-v2.cpp
  gnc-book-xml-v2.cpp
  gnc-budget-xml-v2.cpp
  gnc-commodity-xml-v2.cpp
  gnc-customer-xml-v2.cpp
  gnc-employee-xml-v2.cpp
  gnc-entry-xml-v2.cpp
  gnc-freqspec-xml-v2.cpp
  gnc-invoice-xml-v2.cpp
  gnc-job-xml-v2.cpp
  gnc-lot-xml-v2.cpp
  gnc-order-xml-v2.cpp
  gnc-owner-xml-v2.cpp
  gnc-pricedb-xml-v2.cpp
  gnc-recurrence-xml-v2.cpp
  gnc-schedxaction-xml-v2.cpp
  gnc-tax-table-xml-v2.cpp
  gnc-transaction-xml-v2.cpp
  gnc-vendor-xml-v2.cpp
  gnc-xml-backend.cpp
  gnc-xml-helper.cpp
  io-example-account.cpp
  io-gncxml-gen.cpp
  io-gncxml-v1.cpp
  io-gncxml-v2.cpp
  io-utils.cpp
  sixtp-dom-generators.cpp
  sixtp-dom-parsers.cpp
  sixtp-stack.cpp
  sixtp-to-dom-parser.cpp
  sixtp-utils.cpp
  sixtp.cpp
)

set_local_dist(backend_xml_DIST_local ${backend_xml_utils_SOURCES}
  ${backend_xml_utils_noinst_HEADERS} gnc-backend-xml.cpp CMakeLists.txt
  )
set(backend_xml_DIST ${backend_xml_DIST_local} ${test_backend_xml_DIST} PARENT_SCOPE)

# Add dependency on config.h
set_source_files_properties (${backend_xml_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library	(gnc-backend-xml-utils
  ${backend_xml_utils_SOURCES}
  ${backend_xml_utils_noinst_HEADERS}
)

target_link_libraries(gnc-backend-xml-utils gncmod-engine ${LIBXML2_LDFLAGS} ${ZLIB_LDFLAGS})

target_include_directories (gnc-backend-xml-utils
  PUBLIC  ${LIBXML2_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${ZLIB_INCLUDE_DIRS}
)

target_compile_definitions (gnc-backend-xml-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\" -DU_SHOW_CPLUSPLUS_API=0)

install(TARGETS gnc-backend-xml-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# ----

set(libgncmod_backend_xml_SOURCES gnc-backend-xml.cpp)

set_source_files_properties (${libgncmod_backend_xml_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library(gncmod-backend-xml MODULE ${libgncmod_backend_xml_SOURCES})
target_link_libraries(gncmod-backend-xml gnc-backend-xml-utils gncmod-engine
                        gnc-core-utils ${LIBXML2_LDFLAGS} ${GLIB2_LDFLAGS} ${ZLIB_LIBRARY})

target_compile_definitions (gncmod-backend-xml PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\" -DU_SHOW_CPLUSPLUS_API=0)

set(LIB_DIR ${CMAKE_INSTALL_LIBDIR}/gnucash)
if (WIN32)
  set(LIB_DIR ${CMAKE_INSTALL_BINDIR})
endif()


if (APPLE)
  set_target_properties (gncmod-backend-xml PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

install(TARGETS gncmod-backend-xml
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# ----

# Special normal (non-MODULE) library for cutecash only
add_library(gncmod-backend-xml-utils ${libgncmod_backend_xml_SOURCES})
target_link_libraries(gncmod-backend-xml-utils gnc-backend-xml-utils gncmod-engine
                        gnc-core-utils ${LIBXML2_LDFLAGS} ${GLIB2_LDFLAGS} ${ZLIB_LIBRARY})
target_compile_definitions (gncmod-backend-xml-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\" -DU_SHOW_CPLUSPLUS_API=0)
