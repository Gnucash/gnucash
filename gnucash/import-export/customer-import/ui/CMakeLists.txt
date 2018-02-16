set(customer_import_UI gnc-plugin-customer-import-ui.xml)
FILE(COPY ${customer_import_UI} DESTINATION ${DATADIR_BUILD}/gnucash/ui)

INSTALL(FILES ${customer_import_UI} DESTINATION share/gnucash)

SET_DIST_LIST(customer_import_ui_DIST CMakeLists.txt ${customer_import_UI})
