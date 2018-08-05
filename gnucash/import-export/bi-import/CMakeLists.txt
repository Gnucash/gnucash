set(bi_import_SOURCES
  gnc-plugin-bi-import.c
  gncmod-bi-import.c
  dialog-bi-import-gui.c
  dialog-bi-import-helper.c
  dialog-bi-import.c
)

# Add dependency on config.h
set_source_files_properties (${bi_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(bi_import_noinst_HEADERS
  gnc-plugin-bi-import.h 
  dialog-bi-import-gui.h 
  dialog-bi-import-helper.h 
  dialog-bi-import.h
)

add_library(gncmod-bi-import ${bi_import_noinst_HEADERS} ${bi_import_SOURCES})

target_link_libraries(gncmod-bi-import
  gncmod-gnome-utils
  gncmod-gnome-search
  gncmod-app-utils
  gncmod-engine
  gnc-core-utils
  gnc-module
  gnc-gnome
)

target_include_directories(gncmod-bi-import
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

target_compile_definitions(gncmod-bi-import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.bi-import\")

if (APPLE)
  set_target_properties (gncmod-bi-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-bi-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list(bi_import_DIST CMakeLists.txt README ${bi_import_SOURCES} ${bi_import_noinst_HEADERS})
