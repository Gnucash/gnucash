set(bi_import_SOURCES
  gnc-plugin-bi-import.c
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

add_library(gnc-bi-import ${bi_import_noinst_HEADERS} ${bi_import_SOURCES})

target_link_libraries(gnc-bi-import
  gnc-gnome-utils
  gnc-gnome-search
  gnc-app-utils
  gnc-engine
  gnc-core-utils
  gnc-gnome
)

target_include_directories(gnc-bi-import
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

target_compile_definitions(gnc-bi-import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.bi-import\")

if (APPLE)
  set_target_properties (gnc-bi-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

if (COVERAGE AND LCOV_VERSION VERSION_GREATER_EQUAL "2.0")
  add_coverage_target(gnc-bi-import)
endif()

install(TARGETS gnc-bi-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list(bi_import_DIST CMakeLists.txt README ${bi_import_SOURCES} ${bi_import_noinst_HEADERS})
