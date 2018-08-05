add_subdirectory(valgrind)

set_local_dist(debug_DIST_local CMakeLists.txt )
set(debug_DIST ${debug_DIST_local} ${valgrind_DIST} PARENT_SCOPE)