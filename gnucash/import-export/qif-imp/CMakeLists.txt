# CMakeLists.txt for gnucash/import_export/qif-imp

add_subdirectory(test)

set (qif_import_SOURCES
    dialog-account-picker.c
    assistant-qif-import.c
    gnc-plugin-qif-import.c
    gncmod-qif-import.c
)

# Add dependency on config.h
set_source_files_properties (${qif_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set (qif_import_noinst_HEADERS
    dialog-account-picker.h
    assistant-qif-import.h
    gnc-plugin-qif-import.h
)

add_library	(gncmod-qif-import ${qif_import_SOURCES} ${qif_import_noinst_HEADERS})

target_link_libraries(gncmod-qif-import
    gnc-app-utils
    gncmod-gnome-utils
    gnc-gnome
    gnucash-guile)

target_compile_definitions(gncmod-qif-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import.qif.import\")

if (APPLE)
  set_target_properties (gncmod-qif-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-qif-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.


# Scheme

set (qif_import_SCHEME_0
  string.scm
  )

set (qif_import_SCHEME
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

set(qif_import_SCHEME_2
  qif-import.scm
)

set(GUILE_DEPENDS
  gncmod-qif-import
  gnc-gnome
  scm-core-utils
  scm-gnc-module
  scm-app-utils
  scm-engine
  scm-gnome-utils
  )

gnc_add_scheme_targets(scm-qif-import-0
  "${qif_import_SCHEME_0}"
  "gnucash"
  "${GUILE_DEPENDS}"
  FALSE
)

gnc_add_scheme_targets(scm-qif-import-2
  "${qif_import_SCHEME_2}"
  "gnucash"
  "${GUILE_DEPENDS};scm-qif-import-0"
  FALSE
)

gnc_add_scheme_targets(scm-qif-import
  "${qif_import_SCHEME}"
  "gnucash/qif-import"
  "${GUILE_DEPENDS};scm-qif-import-0"
  TRUE
)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module ("gnucash import-export qif-import" "gnucash qif-import" "scm-qif-import-2" "")
gnc_add_scheme_deprecated_module ("gnucash import-export string" "gnucash string" "scm-qif-import-0" "")

set_local_dist(qif_import_DIST_local CMakeLists.txt file-format.txt ${qif_import_SOURCES} ${qif_import_noinst_HEADERS}
        ${qif_import_SCHEME_0} ${qif_import_SCHEME} ${qif_import_SCHEME_2})
set(qif_import_DIST ${qif_import_DIST_local} ${test_qif_import_DIST} PARENT_SCOPE)
