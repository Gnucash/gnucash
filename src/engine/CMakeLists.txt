# CMakeLists.txt for src/engine

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.engine\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for pow.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-module.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR}) # for <Account.h>
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for iso-4217-currencies.c

SET (libgncmod_engine_HEADERS
  Account.h
  FreqSpec.h
  Recurrence.h
  SchedXaction.h
  SX-book.h
  SX-ttinfo.h
  Query.h
  Scrub.h
  Scrub2.h
  Scrub3.h
  Split.h
  TransLog.h
  Transaction.h
  cap-gains.h
  cashobjects.h
  engine-helpers.h
  glib-helpers.h
  gnc-budget.h
  gnc-commodity.h
  gnc-engine.h
  gnc-event.h
  gnc-hooks.h
  gnc-pricedb.h
  gnc-session.h
  kvp-scm.h
  policy.h
  gncAddress.h
  gncAddressP.h
  gncBillTerm.h
  gncBillTermP.h
  gncBusGuile.h
  gncBusiness.h
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

# Command to generate the swig-engine.c wrapper file
SET (SWIG_ENGINE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-engine.c)
GNC_ADD_SWIG_COMMAND (${SWIG_ENGINE_C} ${CMAKE_CURRENT_SOURCE_DIR}/engine.i ${libgncmod_engine_HEADERS})

# Command to generate the iso-4217-currencies.c file
SET (ISO_4217_C ${CMAKE_CURRENT_BINARY_DIR}/iso-4217-currencies.c)
ADD_CUSTOM_COMMAND (
  OUTPUT ${ISO_4217_C}
  DEPENDS iso-4217-currencies.xml iso-currencies-to-c.xsl
  COMMAND
    ${XSLTPROC} -o ${ISO_4217_C} "${CMAKE_CURRENT_SOURCE_DIR}/iso-currencies-to-c.xsl" "${CMAKE_CURRENT_SOURCE_DIR}/iso-4217-currencies.xml"
)
# Add dependency on iso-4217-currencies.c
SET_SOURCE_FILES_PROPERTIES (gnc-commodity.c PROPERTIES OBJECT_DEPENDS ${ISO_4217_C})

# Add dependency on swig-runtime.h
SET_SOURCE_FILES_PROPERTIES (gnc-hooks.c engine-helpers.c kvp-scm.c glib-helpers.c PROPERTIES OBJECT_DEPENDS ${SWIG_RUNTIME_H})

SET (libgncmod_engine_SOURCES
  Account.c
  Recurrence.c
  Query.c
  SchedXaction.c
  SX-book.c
  SX-ttinfo.c
  Scrub.c
  Scrub2.c
  Scrub3.c
  Split.c
  TransLog.c
  Transaction.c
  cap-gains.c
  cashobjects.c
  gnc-budget.c
  gnc-commodity.c
  gnc-engine.c
  gnc-event.c
  gnc-hooks.c
  gnc-lot.c
  gnc-pricedb.c
  gnc-session.c
  gncmod-engine.c
  kvp-scm.c
  engine-helpers.c
  glib-helpers.c
  policy.c
  ${SWIG_ENGINE_C}
  gncAddress.c
  gncBillTerm.c
  gncBusGuile.c
  gncCustomer.c
  gncEmployee.c
  gncEntry.c
  gncInvoice.c
  gncJob.c
  gncOrder.c
  gncOwner.c
  gncTaxTable.c
  gncVendor.c
)

# Add dependency on config.h
#SET_SOURCE_FILES_PROPERTIES (${libgncmod_engine_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(engine
  ${libgncmod_engine_SOURCES}
  ${libgncmod_engine_HEADERS}
  )
