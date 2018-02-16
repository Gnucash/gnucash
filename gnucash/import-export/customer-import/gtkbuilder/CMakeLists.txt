set(customer_import_GLADE dialog-customer-import-gui.glade)
FILE(COPY ${customer_import_GLADE} DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

INSTALL(FILES ${customer_import_GLADE} DESTINATION share/gnucash)

SET_DIST_LIST(customer_import_glade_DIST CMakeLists.txt ${customer_import_GLADE})
