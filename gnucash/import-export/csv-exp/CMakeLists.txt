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

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-csv-export PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-csv-export
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

SET_DIST_LIST (csv_export_DIST CMakeLists.txt
        ${csv_export_SOURCES} ${csv_export_noinst_HEADERS})
