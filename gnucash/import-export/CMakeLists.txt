# CMakeLists.txt for gnucash/import-export

ADD_SUBDIRECTORY(test)

# ############################################################
ADD_SUBDIRECTORY(aqb)
ADD_SUBDIRECTORY(bi-import)
ADD_SUBDIRECTORY(csv-exp)
ADD_SUBDIRECTORY(csv-imp)
ADD_SUBDIRECTORY(customer-import)
ADD_SUBDIRECTORY(log-replay)
ADD_SUBDIRECTORY(ofx)
ADD_SUBDIRECTORY(qif)
ADD_SUBDIRECTORY(qif-imp)


SET (generic_import_SOURCES
  import-account-matcher.c
  import-commodity-matcher.c
  import-backend.c
  import-format-dialog.c
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

TARGET_LINK_LIBRARIES(gncmod-generic-import gncmod-gnome-utils gncmod-engine ${GTK3_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gncmod-generic-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import\")

TARGET_INCLUDE_DIRECTORIES(gncmod-generic-import PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})


IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-generic-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-generic-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

INSTALL(FILES ${generic_import_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

SET(generic_import_EXTRA_DIST generic-import-design.txt)

SET_LOCAL_DIST(import_export_DIST_local CMakeLists.txt ${generic_import_SOURCES}
        ${generic_import_HEADERS} ${generic_import_noinst_HEADERS}
        ${generic_import_EXTRA_DIST})

SET(import_export_DIST ${import_export_DIST_local} ${aqbanking_DIST} ${bi_import_DIST}
        ${csv_export_DIST} ${csv_import_DIST} ${customer_import_DIST}
        ${log_report_DIST} ${ofx_DIST} ${qif_DIST} ${qif_import_DIST}
        ${test_generic_import_DIST}
        PARENT_SCOPE)
