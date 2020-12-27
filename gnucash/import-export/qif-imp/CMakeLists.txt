# CMakeLists.txt for gnucash/import_export/qif-imp

add_subdirectory(test)

set (qif_import_SOURCES
    dialog-account-picker.c
    assistant-qif-import.c
    gnc-plugin-qif-import.c
)

# Add dependency on config.h
set_source_files_properties (${qif_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set (qif_import_noinst_HEADERS
    dialog-account-picker.h
    assistant-qif-import.h
    gnc-plugin-qif-import.h
)

add_library	(gnc-qif-import ${qif_import_SOURCES} ${qif_import_noinst_HEADERS})

target_link_libraries(gnc-qif-import
    gnc-app-utils
    gnc-gnome-utils
    gnc-gnome
    gnucash-guile)

target_include_directories(gnc-qif-import
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_definitions(gnc-qif-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import.qif.import\")

if (APPLE)
  set_target_properties (gnc-qif-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gnc-qif-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.


# Scheme

set (qif_import_SCHEME_0
  qif-utils.scm
  string.scm
  )

set (qif_import_SCHEME_1
  qif-merge-groups.scm
  qif-objects.scm
)

set (qif_import_SCHEME_2
  qif-guess-map.scm
)

set (qif_import_SCHEME_3
  qif-dialog-utils.scm
)

set (qif_import_SCHEME_4
  qif-to-gnc.scm
  qif-parse.scm
)

set (qif_import_SCHEME_5
  qif-file.scm
)

set (qif_import_SCHEME_6
  qif-import.scm
)

set(GUILE_DEPENDS
  gnc-qif-import
  gnc-gnome
  scm-core-utils
  scm-app-utils
  scm-engine
  scm-gnome-utils
  )

gnc_add_scheme_targets(scm-qif-import-0
    SOURCES "${qif_import_SCHEME_0}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_targets(scm-qif-import-1
    SOURCES "${qif_import_SCHEME_1}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "scm-qif-import-0")

gnc_add_scheme_targets(scm-qif-import-2
    SOURCES "${qif_import_SCHEME_2}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "scm-qif-import-1")

gnc_add_scheme_targets(scm-qif-import-3
    SOURCES "${qif_import_SCHEME_3}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "scm-qif-import-2")

gnc_add_scheme_targets(scm-qif-import-4
    SOURCES "${qif_import_SCHEME_4}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "scm-qif-import-3")

gnc_add_scheme_targets(scm-qif-import-5
    SOURCES "${qif_import_SCHEME_5}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "scm-qif-import-4")

gnc_add_scheme_targets(scm-qif-import
    SOURCES "${qif_import_SCHEME_6}"
    OUTPUT_DIR "gnucash/qif-import"
    DEPENDS "scm-qif-import-5")

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash import-export qif-import"
    NEW_MODULE "gnucash qif-import"
    DEPENDS "scm-qif-import-2"
    MAKE_LINKS)
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash import-export string"
    NEW_MODULE "gnucash string"
    DEPENDS "scm-qif-import-0"
    MAKE_LINKS)

set_local_dist(qif_import_DIST_local CMakeLists.txt file-format.txt ${qif_import_SOURCES} ${qif_import_noinst_HEADERS}
  ${qif_import_SCHEME_0}
  ${qif_import_SCHEME_1}
  ${qif_import_SCHEME_2}
  ${qif_import_SCHEME_3}
  ${qif_import_SCHEME_4}
  ${qif_import_SCHEME_5}
  ${qif_import_SCHEME_6})

set(qif_import_DIST ${qif_import_DIST_local} ${test_qif_import_DIST} PARENT_SCOPE)
