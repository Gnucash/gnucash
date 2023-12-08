# CMakeLists.txt for gnucash/import-export

add_subdirectory(test)

# ############################################################
add_subdirectory(aqb)
add_subdirectory(bi-import)
add_subdirectory(csv-exp)
add_subdirectory(csv-imp)
add_subdirectory(customer-import)
add_subdirectory(log-replay)
add_subdirectory(ofx)
add_subdirectory(qif-imp)


set (generic_import_SOURCES
  import-account-matcher.cpp
  import-commodity-matcher.cpp
  import-backend.cpp
  import-format-dialog.cpp
  import-match-picker.cpp
  import-parse.cpp
  import-utilities.cpp
  import-settings.cpp
  import-main-matcher.cpp
  import-pending-matches.cpp
)

# Add dependency on config.h
set_source_files_properties (${generic_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set (generic_import_HEADERS
  import-parse.h
)

set (generic_import_noinst_HEADERS
  import-account-matcher.h
  import-backend.h
  import-commodity-matcher.h
  import-main-matcher.h
  import-match-picker.h
  import-pending-matches.h
  import-settings.h
  import-utilities.h
)

add_library (gnc-generic-import
  ${generic_import_SOURCES}
  ${generic_import_HEADERS}
  ${generic_import_noinst_HEADERS}
)

target_link_libraries(gnc-generic-import gnc-gnome-utils gnc-engine PkgConfig::GTK3 PkgConfig::GLIB2)

target_compile_definitions (gnc-generic-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import\")

target_include_directories(gnc-generic-import PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})


if (APPLE)
  set_target_properties (gnc-generic-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

if (COVERAGE)
  add_coverage_target(gnc-generic-import)
endif()

install(TARGETS gnc-generic-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(FILES ${generic_import_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

set(generic_import_EXTRA_DIST generic-import-design.txt)

set_local_dist(import_export_DIST_local CMakeLists.txt ${generic_import_SOURCES}
        ${generic_import_HEADERS} ${generic_import_noinst_HEADERS}
        ${generic_import_EXTRA_DIST})

set(import_export_DIST ${import_export_DIST_local} ${aqbanking_DIST} ${bi_import_DIST}
        ${csv_export_DIST} ${csv_import_DIST} ${customer_import_DIST}
        ${log_report_DIST} ${ofx_DIST} ${qif_DIST} ${qif_import_DIST}
        ${test_generic_import_DIST}
        PARENT_SCOPE)
