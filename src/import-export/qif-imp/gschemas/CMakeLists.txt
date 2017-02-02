
set(qif_imp_GSCHEMA org.gnucash.dialogs.import.qif.gschema.xml)

ADD_GSCHEMA_TARGETS(qif-imp-gschema "${qif_imp_GSCHEMA}")

SET_DIST_LIST(qif_import_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.dialogs.import.qif.gschema.xml.in.in)