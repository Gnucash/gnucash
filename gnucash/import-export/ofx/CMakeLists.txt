ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(test)

SET(ofx_SOURCES
  gnc-ofx-import.c
  gnc-ofx-kvp.c
  gncmod-ofx-import.c
  gnc-plugin-ofx.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${ofx_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(ofx_noinst_HEADERS
  gnc-ofx-import.h
  gnc-ofx-kvp.h
  gnc-plugin-ofx.h
)

SET(ofx_UI gnc-plugin-ofx-ui.xml)

IF (WITH_OFX)
  ADD_LIBRARY(gncmod-ofx ${ofx_SOURCES} ${ofx_noinst_HEADERS})

  TARGET_LINK_LIBRARIES(gncmod-ofx gncmod-generic-import gncmod-engine gncmod-app-utils gnc-core-utils
                    gncmod-gnome-utils ${LIBOFX_LDFLAGS})

  TARGET_COMPILE_DEFINITIONS(gncmod-ofx PRIVATE -DG_LOG_DOMAIN=\"gnc.import.ofx\")

  TARGET_INCLUDE_DIRECTORIES(gncmod-ofx PRIVATE ${LIBOFX_INCLUDE_DIRS})

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-ofx PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

  INSTALL(TARGETS gncmod-ofx
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

  INSTALL(FILES ${ofx_UI} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/ui)

  FILE(COPY ${ofx_UI}
    DESTINATION ${DATADIR_BUILD}/gnucash/ui)
ENDIF()

SET_LOCAL_DIST(ofx_DIST_local CMakeLists.txt migratable-prefs-ofx.xml ${ofx_SOURCES} ${ofx_noinst_HEADERS} ${ofx_UI})
SET(ofx_DIST ${ofx_DIST_local} ${test_ofx_DIST} ${ofx_gschema_DIST} PARENT_SCOPE)
