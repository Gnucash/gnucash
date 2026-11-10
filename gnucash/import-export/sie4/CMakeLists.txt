
add_subdirectory(test)

set(sie4_export_SOURCES
  gnc-plugin-sie4-export.c
  dialog-sie4-export.c
  sie4-export.cpp
)

set_source_files_properties (${sie4_export_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(sie4_export_noinst_HEADERS
  gnc-plugin-sie4-export.h
  dialog-sie4-export.h
  sie4-export.h
)

add_library(gnc-sie4-export ${sie4_export_noinst_HEADERS} ${sie4_export_SOURCES})

target_link_libraries(gnc-sie4-export
    gnc-engine
    gnc-gnome-utils
    gnc-app-utils
    gnc-core-utils)

target_include_directories(gnc-sie4-export
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_definitions(gnc-sie4-export PRIVATE -DG_LOG_DOMAIN=\"gnc.export.sie4\")

if (APPLE)
  set_target_properties (gnc-sie4-export PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

if (COVERAGE)
  add_coverage_target(gnc-sie4-export)
endif()

install(TARGETS gnc-sie4-export
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

set_local_dist (sie4_export_DIST_local
  CMakeLists.txt
  ${sie4_export_SOURCES}
  ${sie4_export_noinst_HEADERS}
)

set(sie4_export_DIST
  ${sie4_export_DIST_local}
  ${test_sie4_export_DIST}
  PARENT_SCOPE
)
