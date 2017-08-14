ADD_SUBDIRECTORY(gschemas)

SET(csv_export_SOURCES
  gncmod-csv-export.c
  gnc-plugin-csv-export.c
  assistant-csv-export.c
  csv-tree-export.c
  csv-transactions-export.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${csv_export_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(csv_export_noinst_HEADERS
  gnc-plugin-csv-export.h
  assistant-csv-export.h
  csv-tree-export.h
  csv-transactions-export.h
)

ADD_LIBRARY(gncmod-csv-export ${csv_export_noinst_HEADERS} ${csv_export_SOURCES})

TARGET_LINK_LIBRARIES(gncmod-csv-export gncmod-register-gnome gncmod-register-core gncmod-ledger-core gncmod-engine
                         gnc-module gncmod-gnome-utils gnc-gnome gncmod-app-utils gnc-core-utils)

TARGET_COMPILE_DEFINITIONS(gncmod-csv-export PRIVATE -DG_LOG_DOMAIN=\"gnc.export.csv\")

INSTALL(TARGETS gncmod-csv-export
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install.

SET(csv_export_GLADE assistant-csv-export.glade)

INSTALL(FILES ${csv_export_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET(csv_export_UI gnc-plugin-csv-export-ui.xml)

INSTALL(FILES ${csv_export_UI} DESTINATION share/gnucash/ui)

FILE(COPY ${csv_export_UI}
  DESTINATION ${DATADIR_BUILD}/gnucash/ui)
FILE(COPY ${csv_export_GLADE}
  DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

SET_LOCAL_DIST(csv_export_DIST_local CMakeLists.txt Makefile.am
        ${csv_export_SOURCES} ${csv_export_noinst_HEADERS}
        ${csv_export_GLADE} ${csv_export_UI})
SET(csv_export_DIST ${csv_export_DIST_local} ${csv_exp_gschema_DIST} PARENT_SCOPE)

