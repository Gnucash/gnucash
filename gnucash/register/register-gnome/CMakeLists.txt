add_subdirectory(test)
include(CheckSymbolExists)

#GTK before 3.14 didn't have GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK
check_symbol_exists(GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK gdk/gdktypes.h have_mod_mask)
if (NOT have_mod_mask)
if (MAC_INTEGRATION)
  add_definitions(-DGDK_MODIFIER_INTENT_DEFAULT_MOD_MASK=GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK|GDK_MOD2_MASK|GDK_SUPER_MASK|GDK_HYPER_MASK|GDK_META_MASK)
else()
  add_definitions(-DGDK_MODIFIER_INTENT_DEFAULT_MOD_MASK=GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK|GDK_SUPER_MASK|GDK_HYPER_MASK|GDK_META_MASK)
endif()
endif()

set (register_gnome_SOURCES
  gncmod-register-gnome.c
  combocell-gnome.c
  datecell-gnome.c
  formulacell-gnome.c
  gnucash-color.c
  gnucash-cursor.c
  gnucash-date-picker.c
  gnucash-header.c
  gnucash-item-edit.c
  gnucash-item-list.c
  gnucash-register.c
  gnucash-scrolled-window.c
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
  gnucash-scrolled-window.h
  gnucash-sheet.h
  gnucash-sheetP.h
  gnucash-style.h
  pricecell-gnome.h
  quickfillcell-gnome.h
  table-gnome.h
)

# Add dependency on config.h
set_source_files_properties (${register_gnome_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library (gncmod-register-gnome ${register_gnome_SOURCES} ${register_gnome_noinst_HEADERS})

target_link_libraries(gncmod-register-gnome gncmod-register-core PkgConfig::GTK3)

target_compile_definitions(gncmod-register-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.register.gnome\")

target_include_directories(gncmod-register-gnome
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/register/ledger-core
)

if (APPLE)
  set_target_properties (gncmod-register-gnome PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-register-gnome
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

set_local_dist(register_gnome_DIST_local CMakeLists.txt ${register_gnome_SOURCES} ${register_gnome_noinst_HEADERS})

set(register_gnome_DIST ${register_gnome_DIST_local} ${test_register_gnome_DIST} PARENT_SCOPE)
