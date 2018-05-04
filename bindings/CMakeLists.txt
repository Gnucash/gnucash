add_subdirectory(python)

set_local_dist(bindings_DIST_local CMakeLists.txt )
set(bindings_DIST ${bindings_DIST_local} ${python_bindings_DIST} PARENT_SCOPE)
