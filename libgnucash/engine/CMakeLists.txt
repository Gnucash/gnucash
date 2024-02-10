# CMakeLists.txt for libgnucash/engine

add_subdirectory(test-core)
add_subdirectory(test)
add_subdirectory(mocks)

set(engine_noinst_HEADERS
  AccountP.h
  SplitP.h
  SX-book.h
  SX-ttinfo.h
  TransactionP.h
  gnc-backend-prov.hpp
  gnc-date-p.h
  gnc-int128.hpp
  gnc-lot.h
  gnc-lot-p.h
  gnc-option-date.hpp
  gnc-option-impl.hpp
  gnc-option-ui.hpp
  gnc-option-uitype.hpp
  gnc-optiondb-impl.hpp
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
  ScrubBudget.h
  Split.h
  TransLog.h
  Transaction.h
  cap-gains.h
  cashobjects.h
  engine-helpers.h
  gnc-accounting-period.h
  gnc-aqbanking-templates.h
  gnc-budget.h
  gnc-commodity.h
  gnc-commodity.hpp
  gnc-date.h
  gnc-datetime.hpp
  gnc-engine.h
  gnc-euro.h
  gnc-event.h
  gnc-features.h
  gnc-hooks.h
  gnc-numeric.h
  gnc-numeric.hpp
  gnc-option.hpp
  gnc-optiondb.h
  gnc-optiondb.hpp
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
  kvp-value.hpp
  policy.h
  qof.h
  qof-backend.hpp
  qofbackend.h
  qofbook.h
  qofbook.hpp
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
  Recurrence.cpp
  Query.c
  SchedXaction.c
  SX-book.c
  SX-ttinfo.c
  Scrub.c
  Scrub2.c
  Scrub3.c
  ScrubBusiness.c
  ScrubBudget.c
  Split.cpp
  TransLog.c
  Transaction.cpp
  cap-gains.c
  cashobjects.c
  gnc-accounting-period.c
  gnc-aqbanking-templates.cpp
  gnc-budget.cpp
  gnc-commodity.cpp
  gnc-date.cpp
  gnc-datetime.cpp
  gnc-engine.c
  gnc-euro.c
  gnc-event.c
  gnc-features.cpp
  gnc-hooks.c
  gnc-int128.cpp
  gnc-lot.cpp
  gnc-numeric.cpp
  gnc-option-date.cpp
  gnc-option.cpp
  gnc-option-impl.cpp
  gnc-optiondb.cpp
  gnc-pricedb.cpp
  gnc-rational.cpp
  gnc-session.c
  gnc-timezone.cpp
  gnc-uri-utils.c
  engine-helpers.c
  guid.cpp
  policy.c
  gncAddress.c
  gncBillTerm.c
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
    add_definitions ("-DOS_WIN32")
endif()

# Add dependency on config.h
set_source_files_properties (${engine_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

# Add dependency on iso-4217-currencies.c
set_source_files_properties (gnc-commodity.c PROPERTIES OBJECT_DEPENDS "${ISO_4217_C};${CONFIG_H}")

add_library (gnc-engine
  ${engine_SOURCES}
  ${engine_HEADERS}
  ${engine_noinst_HEADERS}
)

target_sources(gnc-engine
    PRIVATE
        $<$<BOOL:${WIN32}>:qof-win32.cpp>
        $<$<BOOL:${WIN32}>:${CMAKE_SOURCE_DIR}/borrowed/libc/strptime.c>
)

# Add dependency on iso-4217-currencies.c
add_dependencies (gnc-engine iso-4217-c)

target_link_libraries(gnc-engine
    gnc-core-utils
    ${Boost_DATE_TIME_LIBRARY}
    ${Boost_REGEX_LIBRARY}
    ${ICU4C_I18N_LDFLAGS}
    ${REGEX_LDFLAGS}
    ${GMODULE_LDFLAGS}
    PkgConfig::GLIB2
    ${GOBJECT_LDFLAGS}
    $<$<BOOL:${WIN32}>:bcrypt.lib>)

target_compile_definitions (gnc-engine PRIVATE -DG_LOG_DOMAIN=\"gnc.engine\")

target_include_directories (gnc-engine
    PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR} # for iso-4217-currencies.c
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_BINARY_DIR}/common # for config.h
        ${CMAKE_SOURCE_DIR}/common # for platform.h
        ${LIBINTL_INCLUDE_PATH}
        ${REGEX_INCLUDE_PATH}
        ${CMAKE_SOURCE_DIR}/borrowed/libc # for strptime.h
)

if (APPLE)
  set_target_properties (gnc-engine PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

if (COVERAGE)
  add_coverage_target(gnc-engine)
endif()

install(TARGETS gnc-engine
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${engine_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

set(engine_EXTRA_DIST
        design.txt
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
                     ${engine_noinst_HEADERS} ${engine_EXTRA_DIST})
set(engine_DIST
    ${engine_DIST_local}
    ${engine_test_core_DIST}
    ${test_engine_DIST}
    ${engine_mocks_DIST} PARENT_SCOPE)
