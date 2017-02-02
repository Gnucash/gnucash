
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
     gnc-core-utils gnc-module gnc-qof ${GTK2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-log-replay PRIVATE -DG_LOG_DOMAIN=\"gnc.import.log-replay\")

INSTALL(TARGETS gncmod-log-replay
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install.

SET(log_replay_UI gnc-plugin-log-replay-ui.xml)

INSTALL(FILES ${log_replay_UI} DESTINATION share/gnucash/ui)

FILE(COPY ${log_replay_UI}
  DESTINATION ${DATADIR_BUILD}/gnucash/ui)

SET_DIST_LIST(log_report_DIST CMakeLists.txt Makefile.am
        ${log_replay_SOURCES} ${log_replay_noinst_HEADERS} ${log_replay_UI})