
SET(gnome_utils_GSCHEMA org.gnucash.history.gschema.xml org.gnucash.warnings.gschema.xml)

ADD_GSCHEMA_TARGETS(gnome-utils-gschema "${gnome_utils_GSCHEMA}")

SET_DIST_LIST(gnome_utils_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.history.gschema.xml.in.in
        org.gnucash.warnings.gschema.xml.in.in)