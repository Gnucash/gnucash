
SET(csv_exp_GSCHEMA org.gnucash.dialogs.export.csv.gschema.xml)

add_gschema_targets("${csv_exp_GSCHEMA}")

SET_DIST_LIST(csv_exp_gschema_DIST CMakeLists.txt org.gnucash.dialogs.export.csv.gschema.xml.in)
