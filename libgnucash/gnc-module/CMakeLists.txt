# CMakeLists.txt for libgnucash/gnc-module
add_subdirectory(test)
add_subdirectory(example)

set (gnc_module_SOURCES gnc-module.c)

# Add dependency on config.h
set_source_files_properties (${gnc_module_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set (gnc_module_HEADERS
  gnc-module.h
  gnc-module-api.h
)

add_library	(gnc-module
  ${gnc_module_SOURCES}
  ${gnc_module_HEADERS}
)

target_link_libraries(gnc-module ${GMODULE_LDFLAGS} PkgConfig::GLIB2)

target_compile_definitions (gnc-module PRIVATE -DG_LOG_DOMAIN=\"gnc.module\")

target_include_directories (gnc-module
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR}
            ${CMAKE_SOURCE_DIR}/common
            ${CMAKE_BINARY_DIR}/common # for config.h
)

if (COVERAGE)
  add_coverage_target(gnc-module)
endif()

install(TARGETS gnc-module
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${gnc_module_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

set_local_dist(gnc_module_DIST_local CMakeLists.txt ${gnc_module_SOURCES} ${gnc_module_HEADERS}
        README)
set(gnc_module_DIST ${gnc_module_DIST_local} ${test_gnc_module_DIST} ${example_DIST} PARENT_SCOPE)
