set(csv_export_SOURCES
  gnc-plugin-csv-export.c
  assistant-csv-export.c
  csv-tree-export.c
  csv-transactions-export.c
)

# Add dependency on config.h
set_source_files_properties (${csv_export_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(csv_export_noinst_HEADERS
  gnc-plugin-csv-export.h
  assistant-csv-export.h
  csv-tree-export.h
  csv-transactions-export.h
)

add_library(gnc-csv-export ${csv_export_noinst_HEADERS} ${csv_export_SOURCES})

target_link_libraries(gnc-csv-export
    gnc-register-gnome
    gnc-register-core
    gnc-ledger-core
    gnc-engine
    gnc-gnome-utils
    gnc-gnome
    gnc-app-utils
    gnc-core-utils)

target_include_directories(gnc-csv-export
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_definitions(gnc-csv-export PRIVATE -DG_LOG_DOMAIN=\"gnc.export.csv\")

if (APPLE)
  set_target_properties (gnc-csv-export PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gnc-csv-export
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list (csv_export_DIST CMakeLists.txt
        ${csv_export_SOURCES} ${csv_export_noinst_HEADERS})
