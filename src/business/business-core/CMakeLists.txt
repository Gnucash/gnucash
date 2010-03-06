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

SET (SWIG_BUSINESS_CORE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-business-core.c)
GNC_ADD_SWIG_COMMAND (${SWIG_BUSINESS_CORE_C} ${CMAKE_CURRENT_SOURCE_DIR}/business-core.i)

SET (libgnc_business_core_SOURCES
  gncAddress.c
  gncBillTerm.c
  gncBusGuile.c
  gncBusiness.c
  gncCustomer.c
  gncEmployee.c
  gncEntry.c
  gncInvoice.c
  gncJob.c
  gncOrder.c
  gncOwner.c
  gncTaxTable.c
  gncVendor.c
  gncmod-business-core.c
  ${SWIG_BUSINESS_CORE_C}
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
  gncAddress.h
  gncAddressP.h
  gncBillTerm.h
  gncBillTermP.h
  gncBusGuile.h
  gncBusiness.h
  gncBusinessP.h
  gncCustomer.h
  gncCustomerP.h
  gncEmployee.h
  gncEmployeeP.h
  gncEntry.h
  gncEntryP.h
  gncInvoice.h
  gncInvoiceP.h
  gncJob.h
  gncJobP.h
  gncOrder.h
  gncOrderP.h
  gncOwner.h
  gncOwnerP.h
  gncTaxTable.h
  gncTaxTableP.h
  gncVendor.h
  gncVendorP.h
)

ADD_LIBRARY (business-core
  ${libgnc_business_core_SOURCES}
  ${libgnc_business_core_HEADERS}
  ${libgnc_business_core_xml_SOURCES}
)

