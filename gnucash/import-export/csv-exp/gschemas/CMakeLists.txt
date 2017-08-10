
SET(csv_exp_GSCHEMA org.gnucash.dialogs.export.csv.gschema.xml)

ADD_GSCHEMA_TARGETS(csv-exp-gschema "${csv_exp_GSCHEMA}")

SET_DIST_LIST(csv_exp_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.dialogs.export.csv.gschema.xml.in.in)