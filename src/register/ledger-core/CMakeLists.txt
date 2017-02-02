
ADD_SUBDIRECTORY(test)

SET (ledger_core_SOURCES
  gnc-ledger-display.c
  gnc-ledger-display2.c
  gncmod-ledger-core.c
  split-register.c
  split-register-control.c
  split-register-layout.c
  split-register-load.c
  split-register-model.c
  split-register-model-save.c
  split-register-util.c
)

SET (ledger_core_HEADERS
  gnc-ledger-display.h
  gnc-ledger-display2.h
  split-register.h
  split-register-control.h
  split-register-layout.h
  split-register-model.h
  split-register-model-save.h
  split-register-p.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${ledger_core_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY (gncmod-ledger-core ${ledger_core_SOURCES} ${ledger_core_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-ledger-core gncmod-register-gnome gncmod-register-core gncmod-gnome-utils
        gncmod-engine ${GTK2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gncmod-ledger-core PRIVATE -DG_LOG_DOMAIN=\"gnc.register.ledger\")

TARGET_INCLUDE_DIRECTORIES(gncmod-ledger-core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

INSTALL(TARGETS gncmod-ledger-core
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install

SET_LOCAL_DIST(ledger_core_DIST_local CMakeLists.txt Makefile.am ${ledger_core_SOURCES} ${ledger_core_HEADERS})
SET(ledger_core_DIST ${ledger_core_DIST_local} ${test_ledger_core_DIST} PARENT_SCOPE)