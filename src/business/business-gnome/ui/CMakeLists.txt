FILE(COPY .
  DESTINATION ${DATADIR_BUILD}/gnucash/ui
  PATTERN Makefile.* EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DESTINATION share/gnucash
  PATTERN Makefile.* EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE)

SET(business_gnome_UI gnc-plugin-business-ui.xml gnc-plugin-page-invoice-ui.xml gnc-plugin-page-owner-tree-ui.xml)
SET_DIST_LIST(business_gnome_ui_DIST CMakeLists.txt Makefile.am ${business_gnome_UI})