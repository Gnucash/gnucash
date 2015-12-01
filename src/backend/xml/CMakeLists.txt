# CMakeLists.txt for src/backend/xml

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.backend.xml\")

INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBXML2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # when building swig-gnc-module.cpp

# Command to generate the swig-engine.cpp wrapper file
SET (SWIG_GNC_MODULE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-gnc-module.cpp)
GNC_ADD_SWIG_COMMAND (${SWIG_GNC_MODULE_C} ${CMAKE_CURRENT_SOURCE_DIR}/gnc-module.i)

SET (libgnc_backend_xml_SOURCES
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
  gnc-backend-xml.cpp
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_backend_xml_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-xml
  ${libgnc_backend_xml_SOURCES}
  )
