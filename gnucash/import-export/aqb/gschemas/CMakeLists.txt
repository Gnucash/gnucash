
if (WITH_AQBANKING)
  set(aqb_GSCHEMA org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml org.gnucash.GnuCash.dialogs.flicker.gschema.xml)

  add_gschema_targets("${aqb_GSCHEMA}")

  file(READ migratable-prefs.xml migratable-prefs)
  file(APPEND ${DATADIR_BUILD}/${PROJECT_NAME}/migratable-prefs.xml ${migratable-prefs})
endif()

set_dist_list(aqbanking_gschema_DIST
  CMakeLists.txt
  migratable-prefs.xml
  org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml.in
  org.gnucash.GnuCash.dialogs.flicker.gschema.xml.in)
