set (register_core_SOURCES
  doclinkcell.c
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


set (register_core_HEADERS
  doclinkcell.h
  basiccell.h
  cell-factory.h
  cellblock.h
  combocell.h
  completioncell.h
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
set_source_files_properties (${register_core_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library (gnc-register-core ${register_core_SOURCES} ${register_core_HEADERS})

target_link_libraries(gnc-register-core gnc-gnome-utils gnc-app-utils ${GMODULE_LDFLAGS})

target_compile_definitions (gnc-register-core PRIVATE -DG_LOG_DOMAIN=\"gnc.register.core\")

target_include_directories(gnc-register-core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

if (APPLE)
  set_target_properties (gnc-register-core PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gnc-register-core
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

if (COVERAGE)
  add_coverage_target(gnc-register-core)
endif()

install(FILES ${register_core_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

set_local_dist(register_core_DIST_local CMakeLists.txt README ${register_core_SOURCES} ${register_core_HEADERS})

set(register_core_DIST ${register_core_DIST_local} ${test_register_core_DIST} PARENT_SCOPE)
