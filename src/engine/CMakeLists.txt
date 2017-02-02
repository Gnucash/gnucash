# CMakeLists.txt for src/engine

ADD_SUBDIRECTORY(test-core)
ADD_SUBDIRECTORY(test)

SET(engine_noinst_HEADERS
  AccountP.h
  ScrubP.h
  SplitP.h
  SX-book.h
  SX-ttinfo.h
  TransactionP.h
  gnc-hooks-scm.h
  gnc-lot.h
  gnc-lot-p.h
  gnc-pricedb-p.h
  policy-p.h
)

SET (engine_HEADERS
  Account.h
  FreqSpec.h
  Recurrence.h
  SchedXaction.h
  SX-book.h
  SX-ttinfo.h
  Query.h
  Scrub.h
  Scrub2.h
  ScrubBusiness.h
  Scrub3.h
  Split.h
  TransLog.h
  Transaction.h
  cap-gains.h
  cashobjects.h
  engine-helpers.h
  engine-helpers-guile.h
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
  gncIDSearch.h
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

IF (BUILDING_FROM_VCS)
  # Command to generate the swig-engine.c wrapper file
  SET (SWIG_ENGINE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-engine.c)
  GNC_ADD_SWIG_COMMAND (swig-engine-c ${SWIG_ENGINE_C} ${CMAKE_CURRENT_SOURCE_DIR}/engine.i ${engine_HEADERS})
ELSE()
  SET (SWIG_ENGINE_C swig-engine.c)
ENDIF()

# Command to generate the iso-4217-currencies.c file
SET (ISO_4217_C ${CMAKE_CURRENT_BINARY_DIR}/iso-4217-currencies.c)
ADD_CUSTOM_COMMAND (
  OUTPUT ${ISO_4217_C}
  DEPENDS iso-4217-currencies.xml iso-currencies-to-c.xsl
  COMMAND
    ${LIBXSLT_XSLTPROC_EXECUTABLE} -o ${ISO_4217_C} "${CMAKE_CURRENT_SOURCE_DIR}/iso-currencies-to-c.xsl" "${CMAKE_CURRENT_SOURCE_DIR}/iso-4217-currencies.xml"
    )
ADD_CUSTOM_TARGET(iso-4217-c DEPENDS ${ISO_4217_C})
# Add dependency on iso-4217-currencies.c
SET_SOURCE_FILES_PROPERTIES (gnc-commodity.c PROPERTIES OBJECT_DEPENDS "${ISO_4217_C};${CONFIG_H}")

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgncmod_engine_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

# Add dependency on swig-runtime.h
SET_SOURCE_FILES_PROPERTIES (gnc-hooks.c engine-helpers.c kvp-scm.cpp glib-helpers.c PROPERTIES OBJECT_DEPENDS "${CONFIG_H};${SWIG_RUNTIME_H}")

SET (engine_SOURCES
  Account.c
  Recurrence.c
  Query.c
  SchedXaction.c
  SX-book.c
  SX-ttinfo.c
  Scrub.c
  Scrub2.c
  Scrub3.c
  ScrubBusiness.c
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
  gncAddress.c
  gncBillTerm.c
  gncBusGuile.c
  gncBusiness.c
  gncCustomer.c
  gncEmployee.c
  gncEntry.c
  gncIDSearch.c
  gncInvoice.c
  gncJob.c
  gncOrder.c
  gncOwner.c
  gncTaxTable.c
  gncVendor.c
)


ADD_LIBRARY	(gncmod-engine
  ${engine_SOURCES}
  ${engine_HEADERS}
  ${SWIG_ENGINE_C}
  ${engine_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gncmod-engine gnc-core-utils gnc-module gnc-qof ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gncmod-engine PRIVATE -DG_LOG_DOMAIN=\"gnc.engine\")

TARGET_INCLUDE_DIRECTORIES (gncmod-engine
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR} # for iso-4217-currencies.c
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
)

INSTALL(TARGETS gncmod-engine
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

INSTALL(FILES ${engine_HEADERS} DESTINATION include/gnucash)

# Scheme

SET (engine_SCHEME_0
    engine.scm
    business-core.scm
)

SET (engine_SCHEME_1
    commodity-table.scm
    engine-interface.scm
    engine-utilities.scm
    gnc-numeric.scm
)

SET(BACKEND_DEPENDS gncmod-backend-xml-link)


IF(WITH_SQL)
  LIST(APPEND BACKEND_DEPENDS gncmod-backend-dbi-link)
ENDIF(WITH_SQL)

SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    src/gnc-module)
SET(GUILE_LIBRARY_DIRS src/gnc-module)
SET(GUILE_DEPENDS      ${BACKEND_DEPENDS} gnc-module gncmod-engine scm-gnc-module)


GNC_ADD_SCHEME_TARGETS(scm-engine-0
  "${engine_SCHEME_0}"
  gnucash
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-engine-1
  "${engine_SCHEME_1}"
  ""
  ""
  ""
  ""
  "scm-engine-0;${GUILE_DEPENDS}"
  FALSE
)

ADD_CUSTOM_TARGET(scm-engine ALL DEPENDS scm-engine-1 scm-engine-0)

SET(engine_EXTRA_DIST
        business-core.i
        design.txt
        engine.i
        engine-common.i
        extensions.txt
        iso-4217-currencies.xml
        iso-currencies-to-c.xsl
        kvp_doc.txt
        README
        README.query-api
        SX-book-p.h
        )
SET_LOCAL_DIST(engine_DIST_local CMakeLists.txt Makefile.am ${engine_SOURCES} ${engine_HEADERS}
                     ${engine_noinst_HEADERS} ${engine_SCHEME_0} ${engine_SCHEME_1} ${engine_EXTRA_DIST})
SET(engine_DIST ${engine_DIST_local} ${engine_test_core_DIST} ${test_engine_DIST} PARENT_SCOPE)