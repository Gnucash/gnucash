SET(bi_import_SOURCES
  gnc-plugin-bi-import.c
  gncmod-bi-import.c
  dialog-bi-import-gui.c
  dialog-bi-import-helper.c
  dialog-bi-import.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${bi_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(bi_import_noinst_HEADERS
  gnc-plugin-bi-import.h 
  dialog-bi-import-gui.h 
  dialog-bi-import-helper.h 
  dialog-bi-import.h
)

ADD_LIBRARY(gncmod-bi-import ${bi_import_noinst_HEADERS} ${bi_import_SOURCES})

TARGET_LINK_LIBRARIES(gncmod-bi-import
  gncmod-gnome-utils
  gncmod-gnome-search
  gncmod-app-utils
  gncmod-engine
  gnc-core-utils
  gnc-module
  gnc-gnome
)

TARGET_INCLUDE_DIRECTORIES(gncmod-bi-import
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

TARGET_COMPILE_DEFINITIONS(gncmod-bi-import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.bi-import\")

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-bi-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-bi-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

SET_DIST_LIST(bi_import_DIST CMakeLists.txt README ${bi_import_SOURCES} ${bi_import_noinst_HEADERS})
