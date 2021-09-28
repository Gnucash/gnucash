
if (WITH_AQBANKING)
  set(aqb_GSCHEMA org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml org.gnucash.GnuCash.dialogs.flicker.gschema.xml)

  add_gschema_targets("${aqb_GSCHEMA}")

  set(local_migration_files ${gschema_migration_files})
  list(APPEND local_migration_files ${CMAKE_CURRENT_SOURCE_DIR}/migratable-prefs.xml)
  set(gschema_migration_files ${local_migration_files} CACHE INTERNAL "gschema migration files")
endif()

set_dist_list(aqbanking_gschema_DIST
  CMakeLists.txt
  migratable-prefs.xml
  org.gnucash.GnuCash.dialogs.import.hbci.gschema.xml.in
  org.gnucash.GnuCash.dialogs.flicker.gschema.xml.in)
