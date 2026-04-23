include(CheckSymbolExists)

set (register_gnome_SOURCES
  combocell-gnome.cpp
  completioncell-gnome.cpp
  datecell-gnome.cpp
  formulacell-gnome.cpp
  gnucash-color.cpp
  gnucash-cursor.cpp
  gnucash-date-picker.cpp
  gnucash-header.cpp
  gnucash-item-edit.cpp
  gnucash-item-list.cpp
  gnucash-register.cpp
  gnucash-sheet-private.cpp
  gnucash-sheet.cpp
  gnucash-style.cpp
  pricecell-gnome.cpp
  quickfillcell-gnome.cpp
  table-gnome.cpp
)

set (register_gnome_noinst_HEADERS
  formulacell-gnome.hpp
  gnucash-color.h
  gnucash-cursor.hpp
  gnucash-date-picker.hpp
  gnucash-header.hpp
  gnucash-item-edit.hpp
  gnucash-item-list.hpp
  gnucash-register.h
  gnucash-sheet.h
  gnucash-sheetP.hpp
  gnucash-style.h
  pricecell-gnome.hpp
  quickfillcell-gnome.hpp
  table-gnome.hpp
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
