
set(ofx_GSCHEMA org.gnucash.dialogs.import.ofx.gschema.xml)

ADD_GSCHEMA_TARGETS(ofx-gschema "${ofx_GSCHEMA}")

SET_DIST_LIST(ofx_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.dialogs.import.ofx.gschema.xml.in.in)