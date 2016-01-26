ADD_SUBDIRECTORY(gschemas)
#ADD_SUBDIRECTORY(test)

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

ADD_LIBRARY(gncmod-ofx ${ofx_SOURCES} ${ofx_noinst_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-ofx gncmod-generic-import gncmod-engine gncmod-app-utils gnc-core-utils
                    gncmod-gnome-utils gnc-qof ${LIBOFX_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-ofx PRIVATE -DG_LOG_DOMAIN=\"gnc.import.ofx\")

TARGET_INCLUDE_DIRECTORIES(gncmod-ofx PRIVATE ${LIBOFX_INCLUDE_DIRS})

INSTALL(TARGETS gncmod-ofx
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

SET(ofx_UI gnc-plugin-ofx-ui.xml)

INSTALL(FILES ${ofx_UI} DESTINATION share/gnucash/ui)


IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY ${ofx_UI}
    DESTINATION ${DATADIR_BUILD}/gnucash/ui)
ENDIF()
