# CMakeLists.txt for libgnucash/engine

add_subdirectory(test-core)
add_subdirectory(test)

set(engine_noinst_HEADERS
  AccountP.h
  ScrubP.h
  SplitP.h
  SX-book.h
  SX-ttinfo.h
  TransactionP.h
  engine-deprecated.h
  gnc-backend-prov.hpp
  gnc-date-p.h
  gnc-hooks-scm.h
  gnc-int128.hpp
  gnc-lot.h
  gnc-lot-p.h
  gnc-pricedb-p.h
  policy-p.h
  qofbook-p.h
  qofclass-p.h
  qofevent-p.h
  qofobject-p.h
  qofquery-p.h
  qofquerycore-p.h
)

set (engine_HEADERS
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
  gnc-aqbanking-templates.h
  gnc-budget.h
  gnc-commodity.h
  gnc-date.h
  gnc-datetime.hpp
  gnc-engine.h
  gnc-event.h
  gnc-features.h
  gnc-hooks.h
  gnc-numeric.h
  gnc-numeric.hpp
  gnc-pricedb.h
  gnc-rational.hpp
  gnc-rational-rounding.hpp
  gnc-session.h
  gnc-timezone.hpp
  gnc-uri-utils.h
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
  guid.h
  guid.hpp
  kvp-frame.hpp
  kvp-scm.h
  kvp-value.hpp
  policy.h
  qof.h
  qof-backend.hpp
  qofbackend.h
  qofbook.h
  qofbookslots.h
  qofchoice.h
  qofclass.h
  qofevent.h
  qofid-p.h
  qofid.h
  qofinstance-p.h
  qofinstance.h
  qoflog.h
  qofobject.h
  qofquery.h
  qofquerycore.h
  qofsession.h
  qofsession.hpp
  qofutil.h
  qof-gobject.h
  qof-string-cache.h
)

# Command to generate the swig-engine.c wrapper file
gnc_add_swig_guile_command (swig-engine-c
    SWIG_ENGINE_C swig-engine.c
    ${CMAKE_CURRENT_SOURCE_DIR}/engine.i ${engine_HEADERS}
)

# Command to generate the iso-4217-currencies.c file
set (ISO_4217_C ${CMAKE_CURRENT_BINARY_DIR}/iso-4217-currencies.c)
add_custom_command (
  OUTPUT ${ISO_4217_C}
  DEPENDS iso-4217-currencies.xml iso-currencies-to-c.xsl
  COMMAND
    ${LIBXSLT_XSLTPROC_EXECUTABLE} -o ${ISO_4217_C} "${CMAKE_CURRENT_SOURCE_DIR}/iso-currencies-to-c.xsl" "${CMAKE_CURRENT_SOURCE_DIR}/iso-4217-currencies.xml"
    )
add_custom_target(iso-4217-c DEPENDS ${ISO_4217_C})

set (engine_SOURCES
  Account.cpp
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
  engine-deprecated.c
  gnc-aqbanking-templates.cpp
  gnc-budget.c
  gnc-commodity.c
  gnc-date.cpp
  gnc-datetime.cpp
  gnc-engine.c
  gnc-event.c
  gnc-features.c
  gnc-hooks.c
  gnc-int128.cpp
  gnc-lot.c
  gnc-numeric.cpp
  gnc-pricedb.c
  gnc-rational.cpp
  gnc-session.c
  gnc-timezone.cpp
  gnc-uri-utils.c
  gncmod-engine.c
  kvp-scm.cpp
  engine-helpers.c
  glib-helpers.c
  guid.cpp
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
  kvp-frame.cpp
  kvp-value.cpp
  qof-backend.cpp
  qofbook.cpp
  qofchoice.cpp
  qofclass.cpp
  qofevent.cpp
  qofid.cpp
  qofinstance.cpp
  qoflog.cpp
  qofobject.cpp
  qofquery.cpp
  qofquerycore.cpp
  qofsession.cpp
  qofutil.cpp
  qof-string-cache.cpp
)

if (WIN32)
  add_definitions (-DOS_WIN32)
  set (engine_SOURCES ${engine_SOURCES}
  qof-win32.cpp
  ../../borrowed/libc/strptime.c
  )
endif()

# Add dependency on config.h
set_source_files_properties (${engine_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

# Add dependency on iso-4217-currencies.c
set_source_files_properties (gnc-commodity.c PROPERTIES OBJECT_DEPENDS "${ISO_4217_C};${CONFIG_H}")



add_library (gncmod-engine
  ${engine_SOURCES}
  ${engine_HEADERS}
  ${SWIG_ENGINE_C}
  ${engine_noinst_HEADERS}
)

# Add dependency on swig-runtime.h and iso-4217-currencies.c
add_dependencies (gncmod-engine swig-runtime-h iso-4217-c)
if (WIN32)
  set(BCRYPT "bcrypt.lib")
else()
  set(BCRYPT "")
endif()

target_link_libraries(gncmod-engine gnc-core-utils gnc-module ${Boost_DATE_TIME_LIBRARIES}  ${Boost_REGEX_LIBRARIES}  ${ICU4C_I18N_LDFLAGS} ${BCRYPT} ${REGEX_LDFLAGS} ${GMODULE_LDFLAGS} ${GLIB2_LDFLAGS} ${GOBJECT_LDFLAGS} ${GUILE_LDFLAGS})

target_compile_definitions (gncmod-engine PRIVATE -DG_LOG_DOMAIN=\"gnc.engine\")

target_include_directories (gncmod-engine
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR} # for iso-4217-currencies.c
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_BINARY_DIR}/common # for config.h
        ${CMAKE_SOURCE_DIR}/common # for platform.h
        ${GLIB2_INCLUDE_DIRS}
        ${LIBINTL_INCLUDE_PATH}
        ${REGEX_INCLUDE_PATH}
        ${CMAKE_SOURCE_DIR}/borrowed/libc # for strptime.h
)

if (APPLE)
  set_target_properties (gncmod-engine PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-engine
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${engine_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# Scheme

set (engine_SCHEME_0
    engine.scm
    business-core.scm
)

set (engine_SCHEME_1
    commodity-table.scm
    engine-interface.scm
    engine-utilities.scm
    gnc-numeric.scm
)

set(BACKEND_DEPENDS gncmod-backend-xml)
if (WITH_SQL)
    list(APPEND BACKEND_DEPENDS gncmod-backend-dbi)
endif()

set(GUILE_DEPENDS
  ${BACKEND_DEPENDS}
  gncmod-engine
  scm-gnc-module)


gnc_add_scheme_targets(scm-engine-0
  "${engine_SCHEME_0}"
  gnucash
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_targets(scm-engine-1
  "${engine_SCHEME_1}"
  ""
  "scm-engine-0;${GUILE_DEPENDS}"
  FALSE
)

add_custom_target(scm-engine ALL DEPENDS scm-engine-1 scm-engine-0)

set(engine_EXTRA_DIST
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

if (NOT WIN32)
  list(APPEND engine_EXTRA_DIST qof-win32.cpp)
endif()
set_local_dist(engine_DIST_local CMakeLists.txt ${engine_SOURCES} ${engine_HEADERS}
                     ${engine_noinst_HEADERS} ${engine_SCHEME_0} ${engine_SCHEME_1} ${engine_EXTRA_DIST})
set(engine_DIST ${engine_DIST_local} ${engine_test_core_DIST} ${test_engine_DIST} PARENT_SCOPE)
