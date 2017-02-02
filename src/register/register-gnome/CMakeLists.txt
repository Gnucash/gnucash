ADD_SUBDIRECTORY(test)

SET (register_gnome_SOURCES
  gncmod-register-gnome.c
  combocell-gnome.c
  datecell-gnome.c
  formulacell-gnome.c
  gnucash-color.c
  gnucash-cursor.c
  gnucash-date-picker.c
  gnucash-grid.c
  gnucash-header.c
  gnucash-item-edit.c
  gnucash-item-list.c
  gnucash-scrolled-window.c
  gnucash-sheet.c
  gnucash-style.c
  pricecell-gnome.c
  quickfillcell-gnome.c
  table-gnome.c
)

SET (register_gnome_noinst_HEADERS
  formulacell-gnome.h
  gnucash-color.h
  gnucash-cursor.h
  gnucash-date-picker.h
  gnucash-grid.h
  gnucash-header.h
  gnucash-item-edit.h
  gnucash-item-list.h
  gnucash-scrolled-window.h
  gnucash-sheet.h
  gnucash-sheetP.h
  gnucash-style.h
  pricecell-gnome.h
  quickfillcell-gnome.h
  table-gnome.h
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${register_gnome_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY (gncmod-register-gnome ${register_gnome_SOURCES} ${register_gnome_noinst_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-register-gnome gncmod-register-core gnc-qof ${GNOMECANVAS_LDFLAGS} ${GTK2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-register-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.register.gnome\")

TARGET_INCLUDE_DIRECTORIES(gncmod-register-gnome
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${CMAKE_SOURCE_DIR}/src/register/ledger-core ${GNOMECANVAS_INCLUDE_DIRS}
)

INSTALL(TARGETS gncmod-register-gnome
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install

SET_LOCAL_DIST(register_gnome_DIST_local CMakeLists.txt Makefile.am ${register_gnome_SOURCES} ${register_gnome_noinst_HEADERS})

SET(register_gnome_DIST ${register_gnome_DIST_local} ${test_register_gnome_DIST} PARENT_SCOPE)