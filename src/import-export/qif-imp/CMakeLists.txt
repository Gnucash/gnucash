# CMakeLists.txt for src/import_export/qif-imp

ADD_SUBDIRECTORY(gschemas)
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

INSTALL(TARGETS gncmod-qif-import
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install.


# Scheme

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

SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    src/core-utils src/gnc-module src/scm)
SET(GUILE_LIBRARY_DIRS src/core-utils src/gnc-module src/gnome)
SET(GUILE_DEPENDS      gncmod-qif-import scm-core-utils scm-gnc-module scm-scm)

GNC_ADD_SCHEME_TARGETS(scm-qif-import-2
  "${qif_import_SCHEME_2}"
  "gnucash/import-export"
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

GNC_ADD_SCHEME_TARGETS(scm-qif-import
  "${qif_import_SCHEME}"
  "qif-import"
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

SET(qif_import_GLADE assistant-qif-import.glade dialog-account-picker.glade)

INSTALL(FILES ${qif_import_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET(qif_import_UI gnc-plugin-qif-import-ui.xml)

INSTALL(FILES ${qif_import_UI} DESTINATION share/gnucash/ui)

FILE(COPY ${qif_import_UI}
  DESTINATION ${DATADIR_BUILD}/gnucash/ui)
FILE(COPY ${qif_import_GLADE}
  DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

SET_LOCAL_DIST(qif_import_DIST_local CMakeLists.txt Makefile.am file-format.txt ${qif_import_SOURCES} ${qif_import_noinst_HEADERS}
        ${qif_import_SCHEME} ${qif_import_SCHEME_2} ${qif_import_GLADE} ${qif_import_UI})
SET(qif_import_DIST ${qif_import_DIST_local} ${test_qif_import_DIST} ${qif_import_gschema_DIST} PARENT_SCOPE)