
if (WITH_OFX)
  set(ofx_GSCHEMA org.gnucash.GnuCash.dialogs.import.ofx.gschema.xml)

  add_gschema_targets("${ofx_GSCHEMA}")

  file(READ migratable-prefs.xml migratable-prefs)
  file(APPEND ${DATADIR_BUILD}/${PROJECT_NAME}/migratable-prefs.xml ${migratable-prefs})
endif()

set_dist_list(ofx_gschema_DIST
  CMakeLists.txt
  migratable-prefs.xml
  org.gnucash.GnuCash.dialogs.import.ofx.gschema.xml.in)
