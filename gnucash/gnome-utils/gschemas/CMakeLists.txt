
SET(gnome_utils_GSCHEMA org.gnucash.history.gschema.xml org.gnucash.warnings.gschema.xml)

add_gschema_targets("${gnome_utils_GSCHEMA}")

SET_DIST_LIST(gnome_utils_gschema_DIST CMakeLists.txt org.gnucash.history.gschema.xml.in.in
        org.gnucash.warnings.gschema.xml.in.in)
