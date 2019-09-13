
add_subdirectory(test)

set (ledger_core_SOURCES
  gnc-ledger-display.c
  gnc-ledger-display2.c
  gncmod-ledger-core.c
  split-register.c
  split-register-control.c
  split-register-layout.c
  split-register-load.c
  split-register-model.c
  split-register-model-save.c
  split-register-util.c
  gncEntryLedger.c
  gncEntryLedgerControl.c
  gncEntryLedgerDisplay.c
  gncEntryLedgerLayout.c
  gncEntryLedgerLoad.c
  gncEntryLedgerModel.c
)


set (ledger_core_HEADERS
  gnc-ledger-display.h
  gnc-ledger-display2.h
  split-register.h
  split-register-control.h
  split-register-layout.h
  split-register-model.h
  split-register-model-save.h
  split-register-p.h
  gncEntryLedger.h
  gncEntryLedgerP.h
  gncEntryLedgerControl.h
  gncEntryLedgerLayout.h
  gncEntryLedgerModel.h
)

# Add dependency on config.h
set_source_files_properties (${ledger_core_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library (gncmod-ledger-core ${ledger_core_SOURCES} ${ledger_core_HEADERS})

target_link_libraries(gncmod-ledger-core gncmod-register-gnome gncmod-register-core gncmod-gnome-utils
        gncmod-engine gnc-core-utils PkgConfig::GTK3)

target_compile_definitions (gncmod-ledger-core PRIVATE -DG_LOG_DOMAIN=\"gnc.register.ledger\")

target_include_directories(gncmod-ledger-core
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_BINARY_DIR}/gnucash/gnome-utils # for gnc-warnings.h
)

if (APPLE)
  set_target_properties (gncmod-ledger-core PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-ledger-core
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install


set_local_dist(ledger_core_DIST_local CMakeLists.txt ${ledger_core_SOURCES} ${ledger_core_HEADERS})
set(ledger_core_DIST ${ledger_core_DIST_local} ${test_ledger_core_DIST} PARENT_SCOPE)

