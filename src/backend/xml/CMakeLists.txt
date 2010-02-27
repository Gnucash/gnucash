# CMakeLists.txt for src/backend/xml

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.backend.xml\")

INCLUDE_DIRECTORIES (${GCONF2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBGUILE_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBXML2_INCLUDE_DIR})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/../lib/libc) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/libqof/qof)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/core-utils)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/gnc-module)
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/engine)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # when building swig-gnc-module.c

# Command to generate the swig-engine.c wrapper file
SET (SWIG_GNC_MODULE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-gnc-module.c)
GNC_ADD_SWIG_COMMAND (${SWIG_GNC_MODULE_C} ${CMAKE_CURRENT_SOURCE_DIR}/gnc-module.i)

SET (libgnc_backend_xml_SOURCES
  gnc-account-xml-v2.c 
  gnc-book-xml-v2.c 
  gnc-budget-xml-v2.c 
  gnc-commodity-xml-v2.c 
  gnc-freqspec-xml-v2.c 
  gnc-lot-xml-v2.c 
  gnc-pricedb-xml-v2.c 
  gnc-recurrence-xml-v2.c 
  gnc-schedxaction-xml-v2.c 
  gnc-transaction-xml-v2.c 
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
  gnc-backend-xml.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_backend_xml_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gnc-backend-xml
  ${libgnc_backend_xml_SOURCES}
  )
