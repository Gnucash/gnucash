set(customer_import_SOURCES
  gnc-plugin-customer-import.c
  dialog-customer-import-gui.c
  dialog-customer-import.c
)

# Add dependency on config.h
set_source_files_properties (${customer_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})


set(customer_import_noinst_HEADERS
  gnc-plugin-customer-import.h
  dialog-customer-import-gui.h
  dialog-customer-import.h
)

add_library(gnc-customer-import ${customer_import_SOURCES} ${customer_import_noinst_HEADERS})

target_link_libraries(gnc-customer-import
    gnc-gnome
    gnc-gnome-utils
    gnc-app-utils
    gnc-engine
    gnc-core-utils
    PkgConfig::GLIB2)

target_include_directories(gnc-customer-import
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

target_compile_definitions(gnc-customer-import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.customer-import\")

if (APPLE)
  set_target_properties (gnc-customer-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

if (0 AND COVERAGE) # Generates no coverage data
  add_coverage_target(gnc-customer-import)
endif()

install(TARGETS gnc-customer-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list (customer_import_DIST CMakeLists.txt ${customer_import_SOURCES} ${customer_import_noinst_HEADERS})
