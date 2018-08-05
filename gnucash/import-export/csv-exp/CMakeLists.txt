set(csv_export_SOURCES
  gncmod-csv-export.c
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

add_library(gncmod-csv-export ${csv_export_noinst_HEADERS} ${csv_export_SOURCES})

target_link_libraries(gncmod-csv-export gncmod-register-gnome gncmod-register-core gncmod-ledger-core gncmod-engine
                         gnc-module gncmod-gnome-utils gnc-gnome gncmod-app-utils gnc-core-utils)

target_compile_definitions(gncmod-csv-export PRIVATE -DG_LOG_DOMAIN=\"gnc.export.csv\")

if (APPLE)
  set_target_properties (gncmod-csv-export PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-csv-export
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list (csv_export_DIST CMakeLists.txt
        ${csv_export_SOURCES} ${csv_export_noinst_HEADERS})
