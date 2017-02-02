ADD_SUBDIRECTORY(test)

SET (register_core_SOURCES
  gncmod-register-core.c
  basiccell.c
  cell-factory.c
  cellblock.c
  formulacell.c
  gtable.c
  numcell.c
  pricecell.c
  quickfillcell.c
  recncell.c
  checkboxcell.c
  register-common.c
  table-allgui.c
  table-control.c
  table-layout.c
  table-model.c
)  


SET (register_core_HEADERS
  basiccell.h
  cell-factory.h
  cellblock.h
  combocell.h
  datecell.h
  formulacell.h
  gtable.h
  numcell.h
  pricecell.h
  quickfillcell.h
  recncell.h
  checkboxcell.h
  register-common.h
  table-allgui.h
  table-control.h
  table-layout.h
  table-model.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${register_core_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY (gncmod-register-core ${register_core_SOURCES} ${register_core_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-register-core gncmod-gnome-utils  gnc-qof gncmod-app-utils ${GMODULE_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gncmod-register-core PRIVATE -DG_LOG_DOMAIN=\"gnc.register.core\")

TARGET_INCLUDE_DIRECTORIES(gncmod-register-core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

INSTALL(TARGETS gncmod-register-core
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

INSTALL(FILES ${register_core_HEADERS} DESTINATION include/gnucash)

SET_LOCAL_DIST(register_core_DIST_local CMakeLists.txt Makefile.am README ${register_core_SOURCES} ${register_core_HEADERS})

SET(register_core_DIST ${register_core_DIST_local} ${test_register_core_DIST} PARENT_SCOPE)