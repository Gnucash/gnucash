ADD_SUBDIRECTORY(ledger-core)
ADD_SUBDIRECTORY(register-core)
ADD_SUBDIRECTORY(register-gnome)

SET_LOCAL_DIST(register_DIST_local CMakeLists.txt )
SET(register_DIST ${register_DIST_local} ${ledger_core_DIST} ${register_core_DIST} ${register_gnome_DIST} PARENT_SCOPE)