
SET(business_gnome_GSCHEMA org.gnucash.dialogs.business.gschema.xml)

ADD_GSCHEMA_TARGETS(business-gnome-gschema "${business_gnome_GSCHEMA}")

SET_DIST_LIST(business_gnome_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.dialogs.business.gschema.xml.in.in)