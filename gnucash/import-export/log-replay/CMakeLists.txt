
set(log_replay_SOURCES
  gnc-log-replay.c
  gnc-plugin-log-replay.c
)

# Add dependency on config.h
set_source_files_properties (${log_replay_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(log_replay_noinst_HEADERS
  gnc-log-replay.h
  gnc-plugin-log-replay.h
)

add_library(gnc-log-replay ${log_replay_SOURCES} ${log_replay_noinst_HEADERS})

target_link_libraries(gnc-log-replay
    gnc-gnome-utils
    gnc-app-utils
    gnc-engine
    gnc-core-utils
    PkgConfig::GTK3)

target_compile_definitions(gnc-log-replay PRIVATE -DG_LOG_DOMAIN=\"gnc.import.log-replay\")

target_include_directories(gnc-log-replay
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

if (APPLE)
  set_target_properties (gnc-log-replay PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

if (COVERAGE AND LCOV_VERSION VERSION_GREATER_EQUAL "2.0")
  add_coverage_target(gnc-log-replay)
endif()

install(TARGETS gnc-log-replay
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list(log_report_DIST CMakeLists.txt
        ${log_replay_SOURCES} ${log_replay_noinst_HEADERS})
