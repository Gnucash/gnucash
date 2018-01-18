ADD_SUBDIRECTORY(python)

SET_LOCAL_DIST(bindings_DIST_local CMakeLists.txt )
SET(bindings_DIST ${bindings_DIST_local} ${python_bindings_DIST} PARENT_SCOPE)
