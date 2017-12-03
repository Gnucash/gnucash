# CMakeLists.txt for libgnucash/backend/xml

ADD_SUBDIRECTORY(test)

SET (backend_xml_utils_noinst_HEADERS
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

SET (backend_xml_utils_SOURCES
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

SET_LOCAL_DIST(backend_xml_DIST_local ${backend_xml_utils_SOURCES}
  ${backend_xml_utils_noinst_HEADERS} gnc-backend-xml.cpp CMakeLists.txt
  )
SET(backend_xml_DIST ${backend_xml_DIST_local} ${test_backend_xml_DIST} PARENT_SCOPE)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${backend_xml_utils_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-xml-utils
  ${backend_xml_utils_SOURCES}
  ${backend_xml_utils_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gnc-backend-xml-utils gncmod-engine ${LIBXML2_LDFLAGS} ${ZLIB_LDFLAGS})

TARGET_INCLUDE_DIRECTORIES (gnc-backend-xml-utils
  PUBLIC  ${LIBXML2_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${ZLIB_INCLUDE_DIRS}
)

TARGET_COMPILE_DEFINITIONS (gnc-backend-xml-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\" -DU_SHOW_CPLUSPLUS_API=0)

INSTALL(TARGETS gnc-backend-xml-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# ----

SET(libgncmod_backend_xml_SOURCES gnc-backend-xml.cpp)

SET_SOURCE_FILES_PROPERTIES (${libgncmod_backend_xml_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY(gncmod-backend-xml MODULE ${libgncmod_backend_xml_SOURCES})
TARGET_LINK_LIBRARIES(gncmod-backend-xml gnc-backend-xml-utils gncmod-engine
                        gnc-core-utils ${LIBXML2_LDFLAGS} ${GLIB2_LDFLAGS} ${ZLIB_LIBRARY})

TARGET_COMPILE_DEFINITIONS (gncmod-backend-xml PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\" -DU_SHOW_CPLUSPLUS_API=0)

SET(LIB_DIR ${CMAKE_INSTALL_LIBDIR}/gnucash)
IF (WIN32)
  SET(LIB_DIR ${CMAKE_INSTALL_BINDIR})
ENDIF(WIN32)


IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-backend-xml PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
ENDIF()

INSTALL(TARGETS gncmod-backend-xml
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# ----

# Special normal (non-MODULE) library for cutecash only
ADD_LIBRARY(gncmod-backend-xml-utils ${libgncmod_backend_xml_SOURCES})
TARGET_LINK_LIBRARIES(gncmod-backend-xml-utils gnc-backend-xml-utils gncmod-engine
                        gnc-core-utils ${LIBXML2_LDFLAGS} ${GLIB2_LDFLAGS} ${ZLIB_LIBRARY})
TARGET_COMPILE_DEFINITIONS (gncmod-backend-xml-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\" -DU_SHOW_CPLUSPLUS_API=0)
