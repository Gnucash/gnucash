# CMakeLists.txt for src/import-export

ADD_SUBDIRECTORY(test)

# ############################################################
ADD_SUBDIRECTORY(aqb)
ADD_SUBDIRECTORY(csv-exp)
ADD_SUBDIRECTORY(csv-imp)
ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(log-replay)
ADD_SUBDIRECTORY(ofx)
ADD_SUBDIRECTORY(qif)
ADD_SUBDIRECTORY(qif-imp)


SET (generic_import_SOURCES
  import-account-matcher.c
  import-commodity-matcher.c
  import-backend.c
  import-format-dialog.c
  import-match-map.c
  import-match-picker.c
  import-parse.c
  import-utilities.c
  import-settings.c
  import-main-matcher.c
  import-pending-matches.c
  gncmod-generic-import.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${generic_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (generic_import_HEADERS
  import-parse.h
  import-match-map.h
)

SET (generic_import_noinst_HEADERS
  import-account-matcher.h
  import-backend.h
  import-commodity-matcher.h
  import-main-matcher.h
  import-match-picker.h
  import-pending-matches.h
  import-settings.h
  import-utilities.h
)

ADD_LIBRARY (gncmod-generic-import
  ${generic_import_SOURCES}
  ${generic_import_HEADERS}
  ${generic_import_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gncmod-generic-import gncmod-gnome-utils gncmod-engine ${GTK2_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gncmod-generic-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import\")

TARGET_INCLUDE_DIRECTORIES(gncmod-generic-import PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})


INSTALL(TARGETS gncmod-generic-import
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin
)

INSTALL(FILES ${generic_import_HEADERS} DESTINATION include/gnucash)

SET(generic_import_GLADE dialog-import.glade)

INSTALL(FILES ${generic_import_GLADE} DESTINATION share/gnucash/gtkbuilder)


FILE(COPY ${generic_import_GLADE} DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

SET(generic_import_EXTRA_DIST generic-import-design.txt)

SET_LOCAL_DIST(import_export_DIST_local CMakeLists.txt Makefile.am ${generic_import_SOURCES}
        ${generic_import_HEADERS} ${generic_import_GLADE} ${generic_import_noinst_HEADERS}
        ${generic_import_EXTRA_DIST})

SET(import_export_DIST ${import_export_DIST_local} ${aqbanking_DIST}
        ${csv_export_DIST} ${csv_import_DIST}
        ${generic_import_gschema_DIST} ${log_report_DIST} ${ofx_DIST} ${qif_DIST} ${qif_import_DIST}
        ${test_generic_import_DIST}
        PARENT_SCOPE)
