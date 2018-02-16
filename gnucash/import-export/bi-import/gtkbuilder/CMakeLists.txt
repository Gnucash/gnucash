set(bi_import_GLADE dialog-bi-import-gui.glade)

FILE(COPY ${bi_import_GLADE} DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

INSTALL(FILES ${bi_import_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET_DIST_LIST(bi_import_glade_DIST CMakeLists.txt ${bi_import_GLADE})
