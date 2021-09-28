
if (WITH_OFX)
  set(ofx_GSCHEMA org.gnucash.GnuCash.dialogs.import.ofx.gschema.xml)

  add_gschema_targets("${ofx_GSCHEMA}")

  set(local_migration_files ${gschema_migration_files})
  list(APPEND local_migration_files ${CMAKE_CURRENT_SOURCE_DIR}/migratable-prefs.xml)
  set(gschema_migration_files ${local_migration_files} CACHE INTERNAL "gschema migration files")
endif()

set_dist_list(ofx_gschema_DIST
  CMakeLists.txt
  migratable-prefs.xml
  org.gnucash.GnuCash.dialogs.import.ofx.gschema.xml.in)
