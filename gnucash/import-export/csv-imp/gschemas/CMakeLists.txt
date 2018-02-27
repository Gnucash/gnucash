SET(csv_imp_GSCHEMA org.gnucash.dialogs.import.csv.gschema.xml)

add_gschema_targets("${csv_imp_GSCHEMA}")

SET_DIST_LIST(csv_import_gschema_DIST CMakeLists.txt org.gnucash.dialogs.import.csv.gschema.xml.in)
