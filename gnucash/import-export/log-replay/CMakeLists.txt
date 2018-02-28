
SET(log_replay_SOURCES
  gnc-log-replay.c
  gnc-plugin-log-replay.c
  gncmod-log-replay.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${log_replay_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(log_replay_noinst_HEADERS
  gnc-log-replay.h
  gnc-plugin-log-replay.h
)

ADD_LIBRARY(gncmod-log-replay ${log_replay_SOURCES} ${log_replay_noinst_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-log-replay gncmod-gnome-utils gncmod-app-utils gncmod-engine
     gnc-core-utils gnc-module ${GTK3_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-log-replay PRIVATE -DG_LOG_DOMAIN=\"gnc.import.log-replay\")

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-log-replay PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-log-replay
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

SET_DIST_LIST(log_report_DIST CMakeLists.txt 
        ${log_replay_SOURCES} ${log_replay_noinst_HEADERS})
