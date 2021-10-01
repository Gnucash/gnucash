
if (WITH_AQBANKING)
  set(aqb_GSCHEMA org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml org.gnucash.GnuCash.dialogs.flicker.gschema.xml)

  add_gschema_targets("${aqb_GSCHEMA}")

  set(local_preftrans_files ${gschema_preftrans_files})
  list(APPEND local_preftrans_files ${CMAKE_CURRENT_SOURCE_DIR}/pref_transformations.xml)
  set(gschema_preftrans_files ${local_preftrans_files} CACHE INTERNAL "files describing transformations for our gsettings based preferences")
endif()

set_dist_list(aqbanking_gschema_DIST
  CMakeLists.txt
  pref_transformations.xml
  org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml.in
  org.gnucash.GnuCash.dialogs.flicker.gschema.xml.in)
