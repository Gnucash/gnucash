
set(qif_imp_GSCHEMA org.gnucash.dialogs.import.qif.gschema.xml)

add_gschema_targets("${qif_imp_GSCHEMA}")

SET_DIST_LIST(qif_import_gschema_DIST CMakeLists.txt org.gnucash.dialogs.import.qif.gschema.xml.in)
