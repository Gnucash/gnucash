
IF (WITH_AQBANKING)
  SET(aqb_GSCHEMA org.gnucash.dialogs.import.hbci.gschema.xml)
  ADD_GSCHEMA_TARGETS(aqb-gschema "${aqb_GSCHEMA}")
ENDIF(WITH_AQBANKING)  

SET_DIST_LIST(aqbanking_gschema_DIST CMakeLists.txt Makefile.am org.gnucash.dialogs.import.hbci.gschema.xml.in.in)