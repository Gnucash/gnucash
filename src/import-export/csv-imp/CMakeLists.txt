ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(test)

SET(csv_import_SOURCES
  gncmod-csv-import.c
  assistant-csv-account-import.c
  assistant-csv-trans-import.c
  gnc-plugin-csv-import.c
  csv-account-import.c
  gnc-csv-model.c
  gnc-csv-gnumeric-popup.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${csv_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(csv_import_noinst_HEADERS
  assistant-csv-account-import.h
  assistant-csv-trans-import.h
  gnc-plugin-csv-import.h
  csv-account-import.h
  gnc-csv-model.h
  gnc-csv-gnumeric-popup.h
)

ADD_LIBRARY(gncmod-csv-import ${csv_import_noinst_HEADERS} ${csv_import_SOURCES}
        ${CMAKE_SOURCE_DIR}/lib/stf/stf-parse.h ${CMAKE_SOURCE_DIR}/lib/stf/stf-parse.c)

TARGET_LINK_LIBRARIES(gncmod-csv-import ${GOFFICE_LDFLAGS} gncmod-generic-import gncmod-gnome-utils
                 gncmod-app-utils gncmod-engine gnc-core-utils gnc-module)


TARGET_COMPILE_DEFINITIONS(gncmod-csv-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import.csv\")

TARGET_INCLUDE_DIRECTORIES(gncmod-csv-import PRIVATE
     ${CMAKE_SOURCE_DIR}/lib ${GOFFICE_INCLUDE_DIRS}
)

INSTALL(TARGETS gncmod-csv-import
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)

# No headers to install

SET(csv_import_GLADE assistant-csv-account-import.glade
      assistant-csv-trans-import.glade)

INSTALL(FILES ${csv_import_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET(csv_import_UI gnc-plugin-csv-import-ui.xml)

INSTALL(FILES ${csv_import_UI} DESTINATION share/gnucash/ui)

FILE(COPY ${csv_import_UI}
  DESTINATION ${DATADIR_BUILD}/gnucash/ui)
FILE(COPY ${csv_import_GLADE}
  DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

SET_LOCAL_DIST(csv_import_DIST_local CMakeLists.txt Makefile.am
        ${csv_import_SOURCES} ${csv_import_noinst_HEADERS}
        ${csv_import_GLADE} ${csv_import_UI})
SET(csv_import_DIST ${csv_import_DIST_local} ${test_csv_import_DIST} ${csv_import_gschema_DIST} PARENT_SCOPE)