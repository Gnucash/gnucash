
IF (WITH_AQBANKING)
  SET(aqb_GSCHEMA org.gnucash.dialogs.import.hbci.gschema.xml)

  add_gschema_targets("${aqb_GSCHEMA}")
ENDIF(WITH_AQBANKING)

SET_DIST_LIST(aqbanking_gschema_DIST CMakeLists.txt org.gnucash.dialogs.import.hbci.gschema.xml.in)
