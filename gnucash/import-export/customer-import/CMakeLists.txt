set(customer_import_SOURCES
  gnc-plugin-customer-import.c
  gncmod-customer-import.c
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

add_library(gncmod-customer-import ${customer_import_SOURCES} ${customer_input_noinst_HEADERS})

target_link_libraries(gncmod-customer-import gnc-gnome gncmod-gnome-utils gncmod-app-utils
     gncmod-engine gnc-core-utils gnc-module ${GLIB2_LDFLAGS})

target_include_directories(gncmod-customer-import
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

target_compile_definitions(gncmod-customer-import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.customer-import\")

if (APPLE)
  set_target_properties (gncmod-customer-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-customer-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list (customer_import_DIST CMakeLists.txt ${customer_import_SOURCES} ${customer_import_noinst_HEADERS})
