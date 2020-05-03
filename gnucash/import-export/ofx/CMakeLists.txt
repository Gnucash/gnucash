add_subdirectory(gschemas)
add_subdirectory(test)

set(ofx_SOURCES
  gnc-ofx-import.c
  gncmod-ofx-import.c
  gnc-plugin-ofx.c
)

# Add dependency on config.h
set_source_files_properties (${ofx_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(ofx_noinst_HEADERS
  gnc-ofx-import.h
  gnc-plugin-ofx.h
)

set(ofx_UI gnc-plugin-ofx-ui.xml)

if (WITH_OFX)
  add_library(gncmod-ofx ${ofx_SOURCES} ${ofx_noinst_HEADERS})

  target_link_libraries(gncmod-ofx gncmod-generic-import gncmod-engine gncmod-app-utils gnc-core-utils
                    gncmod-gnome-utils ${LIBOFX_LDFLAGS})

  target_compile_definitions(gncmod-ofx PRIVATE -DG_LOG_DOMAIN=\"gnc.import.ofx\")

  target_include_directories(gncmod-ofx PRIVATE ${LIBOFX_INCLUDE_DIRS})

if (APPLE)
  set_target_properties (gncmod-ofx PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

  install(TARGETS gncmod-ofx
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

  install(FILES ${ofx_UI} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/ui)

  file(COPY ${ofx_UI}
    DESTINATION ${DATADIR_BUILD}/gnucash/ui)
endif()

set_local_dist(ofx_DIST_local CMakeLists.txt migratable-prefs-ofx.xml ${ofx_SOURCES} ${ofx_noinst_HEADERS} ${ofx_UI})
set(ofx_DIST ${ofx_DIST_local} ${test_ofx_DIST} ${ofx_gschema_DIST} PARENT_SCOPE)
