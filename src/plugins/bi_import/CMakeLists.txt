
ADD_SUBDIRECTORY(gtkbuilder)
ADD_SUBDIRECTORY(ui)

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

ADD_LIBRARY(gncmod-bi_import ${bi_import_noinst_HEADERS} ${bi_import_SOURCES})

TARGET_LINK_LIBRARIES(gncmod-bi_import
  gncmod-business-gnome
  gncmod-gnome-utils
  gncmod-gnome-search
  gncmod-app-utils
  gncmod-engine
  gnc-core-utils
  gnc-module
  gnc-qof
)

TARGET_COMPILE_DEFINITIONS(gncmod-bi_import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.bi-import\")

INSTALL(TARGETS gncmod-bi_import
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install.

SET_LOCAL_DIST(bi_import_DIST_local CMakeLists.txt Makefile.am README ${bi_import_SOURCES} ${bi_import_noinst_HEADERS})
SET(bi_import_DIST ${bi_import_DIST_local} ${bi_import_ui_DIST} ${bi_import_glade_DIST} PARENT_SCOPE)