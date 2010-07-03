# CMakeLists.txt for src/business/business-code

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.business.core\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBXML2_INCLUDE_DIR})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module )
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine )
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src/engine) # for swig-runtime.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/backend/xml) # for io-gncxml-v2.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # when building swig-core-utils.c
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for gncla-dir.h

SET (libgnc_business_core_SOURCES
  gncmod-business-core.c
)

SET (libgnc_business_core_xml_SOURCES
  xml/gnc-address-xml-v2.c
  xml/gnc-bill-term-xml-v2.c
  xml/gnc-customer-xml-v2.c
  xml/gnc-employee-xml-v2.c
  xml/gnc-entry-xml-v2.c
  xml/gnc-invoice-xml-v2.c
  xml/gnc-job-xml-v2.c
  xml/gnc-order-xml-v2.c
  xml/gnc-owner-xml-v2.c
  xml/gnc-tax-table-xml-v2.c
  xml/gnc-vendor-xml-v2.c
  xml/gncmod-business-backend-xml.c
)

SET (libgnc_business_core_HEADERS
)

ADD_LIBRARY (business-core
  ${libgnc_business_core_SOURCES}
  ${libgnc_business_core_HEADERS}
  ${libgnc_business_core_xml_SOURCES}
)

