add_subdirectory(guile)
add_subdirectory(python)

set_local_dist(bindings_DIST_local
    CMakeLists.txt
    app-utils.i
    business-core.i
    core-utils.i
    engine.i
    engine-common.i)
set(bindings_DIST ${bindings_DIST_local} ${guile_DIST} ${python_bindings_DIST} PARENT_SCOPE)
