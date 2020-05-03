
set(log_replay_SOURCES
  gnc-log-replay.c
  gnc-plugin-log-replay.c
  gncmod-log-replay.c
)

# Add dependency on config.h
set_source_files_properties (${log_replay_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(log_replay_noinst_HEADERS
  gnc-log-replay.h
  gnc-plugin-log-replay.h
)

add_library(gncmod-log-replay ${log_replay_SOURCES} ${log_replay_noinst_HEADERS})

target_link_libraries(gncmod-log-replay gncmod-gnome-utils gncmod-app-utils gncmod-engine
     gnc-core-utils gnc-module PkgConfig::GTK3)

target_compile_definitions(gncmod-log-replay PRIVATE -DG_LOG_DOMAIN=\"gnc.import.log-replay\")

if (APPLE)
  set_target_properties (gncmod-log-replay PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-log-replay
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_dist_list(log_report_DIST CMakeLists.txt
        ${log_replay_SOURCES} ${log_replay_noinst_HEADERS})
