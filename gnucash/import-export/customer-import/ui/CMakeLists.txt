FILE(COPY .
  DESTINATION ${DATADIR_BUILD}/gnucash/ui
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION share/gnucash
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE)

SET_DIST_LIST(customer_import_ui_DIST CMakeLists.txt gnc-plugin-customer-import-ui.xml)
