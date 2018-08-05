add_subdirectory(ledger-core)
add_subdirectory(register-core)
add_subdirectory(register-gnome)

set_local_dist(register_DIST_local CMakeLists.txt )
set(register_DIST ${register_DIST_local} ${ledger_core_DIST} ${register_core_DIST} ${register_gnome_DIST} PARENT_SCOPE)