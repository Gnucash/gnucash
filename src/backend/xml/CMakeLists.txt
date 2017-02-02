# CMakeLists.txt for src/backend/xml

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
  gnc-account-xml-v2.c
  gnc-address-xml-v2.c
  gnc-bill-term-xml-v2.c
  gnc-book-xml-v2.c
  gnc-budget-xml-v2.c
  gnc-commodity-xml-v2.c
  gnc-customer-xml-v2.c
  gnc-employee-xml-v2.c
  gnc-entry-xml-v2.c
  gnc-freqspec-xml-v2.c
  gnc-invoice-xml-v2.c
  gnc-job-xml-v2.c
  gnc-lot-xml-v2.c
  gnc-order-xml-v2.c
  gnc-owner-xml-v2.c
  gnc-pricedb-xml-v2.c
  gnc-recurrence-xml-v2.c
  gnc-schedxaction-xml-v2.c
  gnc-tax-table-xml-v2.c
  gnc-transaction-xml-v2.c
  gnc-vendor-xml-v2.c
  gnc-xml-helper.c
  io-example-account.c
  io-gncxml-gen.c
  io-gncxml-v1.c
  io-gncxml-v2.c
  io-utils.c
  sixtp-dom-generators.c
  sixtp-dom-parsers.c
  sixtp-stack.c
  sixtp-to-dom-parser.c
  sixtp-utils.c
  sixtp.c
)

SET_LOCAL_DIST(backend_xml_DIST_local ${backend_xml_utils_SOURCES} ${backend_xml_utils_noinst_HEADERS}
        gnc-backend-xml.c CMakeLists.txt Makefile.am)
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

TARGET_COMPILE_DEFINITIONS (gnc-backend-xml-utils PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\")

INSTALL(TARGETS gnc-backend-xml-utils
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin)
# No headers to install

# ----

SET(libgncmod_backend_xml_SOURCES gnc-backend-xml.c)

SET_SOURCE_FILES_PROPERTIES (${libgncmod_backend_xml_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY(gncmod-backend-xml MODULE ${libgncmod_backend_xml_SOURCES})
TARGET_LINK_LIBRARIES(gncmod-backend-xml gnc-backend-xml-utils gncmod-engine
                        gnc-core-utils ${LIBXML2_LDFLAGS} ${GLIB2_LDFLAGS} ${ZLIB_LIBRARY})

TARGET_COMPILE_DEFINITIONS (gncmod-backend-xml PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.xml\")

SET(LIB_DIR lib/gnucash)
IF (WIN32)
  SET(LIB_DIR bin)
ENDIF(WIN32)


INSTALL(TARGETS gncmod-backend-xml
  LIBRARY DESTINATION ${LIB_DIR}
  ARCHIVE DESTINATION ${LIB_DIR}
  RUNTIME DESTINATION bin)

# ----

# FIXME: Below is a hack to create .libs/libgncmod-backend-xml to silence some complaints
# from guile when compiling. Eventually, fix the guile module loader to not expect items
# to be in .libs. At this time, WIN32 does not use guile-2, so no need for this.

IF (NOT WIN32)
  SET(_LINK_TARGET ${CMAKE_CURRENT_BINARY_DIR}/.libs/libgncmod-backend-xml${CMAKE_SHARED_MODULE_SUFFIX})
  FILE(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/.libs)

  ADD_CUSTOM_COMMAND(
      OUTPUT ${_LINK_TARGET}
      COMMAND ${CMAKE_COMMAND} -E create_symlink $<TARGET_FILE:gncmod-backend-xml> ${_LINK_TARGET}
      DEPENDS gnc-backend-xml-utils
  )

  ADD_CUSTOM_TARGET(gncmod-backend-xml-link ALL DEPENDS gncmod-backend-xml ${_LINK_TARGET})
ENDIF()
