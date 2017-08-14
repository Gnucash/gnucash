SET(csv_imp_GSCHEMA org.gnucash.dialogs.import.csv.gschema.xml)

ADD_GSCHEMA_TARGETS(csv-imp-gschema "${csv_imp_GSCHEMA}")

SET_DIST_LIST(csv_import_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.dialogs.import.csv.gschema.xml.in.in)