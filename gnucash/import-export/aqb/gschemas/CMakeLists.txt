
if (WITH_AQBANKING)
  set(aqb_GSCHEMA org.gnucash.dialogs.import.hbci.gschema.xml)

  add_gschema_targets("${aqb_GSCHEMA}")
endif()

set_dist_list(aqbanking_gschema_DIST CMakeLists.txt org.gnucash.dialogs.import.hbci.gschema.xml.in)
