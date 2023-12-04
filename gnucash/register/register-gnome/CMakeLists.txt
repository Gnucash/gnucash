include(CheckSymbolExists)

set (register_gnome_SOURCES
  combocell-gnome.c
  completioncell-gnome.c
  datecell-gnome.c
  formulacell-gnome.c
  gnucash-color.c
  gnucash-cursor.c
  gnucash-date-picker.c
  gnucash-header.c
  gnucash-item-edit.c
  gnucash-item-list.c
  gnucash-register.c
  gnucash-sheet-private.c
  gnucash-sheet.c
  gnucash-style.c
  pricecell-gnome.c
  quickfillcell-gnome.c
  table-gnome.c
)

set (register_gnome_noinst_HEADERS
  formulacell-gnome.h
  gnucash-color.h
  gnucash-cursor.h
  gnucash-date-picker.h
  gnucash-header.h
  gnucash-item-edit.h
  gnucash-item-list.h
  gnucash-register.h
  gnucash-sheet.h
  gnucash-sheetP.h
  gnucash-style.h
  pricecell-gnome.h
  quickfillcell-gnome.h
  table-gnome.h
)

# Add dependency on config.h
set_source_files_properties (${register_gnome_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library (gnc-register-gnome ${register_gnome_SOURCES} ${register_gnome_noinst_HEADERS})

target_link_libraries(gnc-register-gnome gnc-register-core PkgConfig::GTK3)

target_compile_definitions(gnc-register-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.register.gnome\")

target_include_directories(gnc-register-gnome
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
)

if (APPLE)
  set_target_properties (gnc-register-gnome PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

if (COVERAGE)
  add_coverage_target(gnc-register-gnome)
endif()

install(TARGETS gnc-register-gnome
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

set_local_dist(register_gnome_DIST_local CMakeLists.txt ${register_gnome_SOURCES} ${register_gnome_noinst_HEADERS})

set(register_gnome_DIST ${register_gnome_DIST_local} ${test_register_gnome_DIST} PARENT_SCOPE)
