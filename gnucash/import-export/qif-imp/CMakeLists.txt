# CMakeLists.txt for gnucash/import_export/qif-imp

ADD_SUBDIRECTORY(test)

SET (qif_import_SOURCES
    dialog-account-picker.c
    assistant-qif-import.c
    gnc-plugin-qif-import.c
    gncmod-qif-import.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${qif_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (qif_import_noinst_HEADERS
    dialog-account-picker.h
    assistant-qif-import.h
    gnc-plugin-qif-import.h
)

ADD_LIBRARY	(gncmod-qif-import ${qif_import_SOURCES} ${qif_import_noinst_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-qif-import gncmod-app-utils gncmod-gnome-utils gnc-gnome)

TARGET_COMPILE_DEFINITIONS(gncmod-qif-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import.qif.import\")

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-qif-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-qif-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.


# Scheme

SET (qif_import_SCHEME_0
  string.scm
  )

SET (qif_import_SCHEME
  qif-dialog-utils.scm
  qif-file.scm
  qif-guess-map.scm
  qif-merge-groups.scm
  qif-objects.scm
  qif-parse.scm
  qif-to-gnc.scm
  qif-utils.scm
  qif-import.scm # yes, included in both SETs
)

SET(qif_import_SCHEME_2
  qif-import.scm
)

SET(GUILE_DEPENDS
  gncmod-qif-import
  gnc-gnome
  scm-core-utils
  scm-gnc-module
  scm-scm
  scm-app-utils
  scm-engine
  scm-gnome-utils
  )

GNC_ADD_SCHEME_TARGETS(scm-qif-import-0
  "${qif_import_SCHEME_0}"
  "gnucash/import-export"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-qif-import-2
  "${qif_import_SCHEME_2}"
  "gnucash/import-export"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-qif-import
  "${qif_import_SCHEME}"
  "qif-import"
  "${GUILE_DEPENDS};scm-qif-import-0"
  FALSE
)

SET_LOCAL_DIST(qif_import_DIST_local CMakeLists.txt file-format.txt ${qif_import_SOURCES} ${qif_import_noinst_HEADERS}
        ${qif_import_SCHEME_0} ${qif_import_SCHEME} ${qif_import_SCHEME_2})
SET(qif_import_DIST ${qif_import_DIST_local} ${test_qif_import_DIST} PARENT_SCOPE)
