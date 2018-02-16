set(bi_import_UI gnc-plugin-bi-import-ui.xml)

FILE(COPY ${bi_import_UI} DESTINATION ${DATADIR_BUILD}/gnucash/ui)

INSTALL(FILES ${bi_import_UI} DESTINATION share/gnucash/ui)

SET_DIST_LIST(bi_import_ui_DIST CMakeLists.txt gnc-plugin-bi-import-ui.xml)
