
IF (WITH_OFX)
  set(ofx_GSCHEMA org.gnucash.dialogs.import.ofx.gschema.xml)

  add_gschema_targets("${ofx_GSCHEMA}")
ENDIF (WITH_OFX)

SET_DIST_LIST(ofx_gschema_DIST CMakeLists.txt org.gnucash.dialogs.import.ofx.gschema.xml.in)
