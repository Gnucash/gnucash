
if (WITH_AQBANKING)
  set(aqb_GSCHEMA org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml org.gnucash.GnuCash.dialogs.flicker.gschema.xml)

  add_gschema_targets("${aqb_GSCHEMA}")
endif()

set_dist_list(aqbanking_gschema_DIST CMakeLists.txt org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml.in org.gnucash.GnuCash.dialogs.flicker.gschema.xml.in)
