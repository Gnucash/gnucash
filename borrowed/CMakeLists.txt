add_subdirectory(libc)
add_subdirectory(goffice)
add_subdirectory(d3-3)
add_subdirectory(chartjs-2)
add_subdirectory(chartjs-4)
add_subdirectory(guile-json)
add_subdirectory(d3-sankey)

set_local_dist(borrowed_DIST_local jenny/jenny.c CMakeLists.txt README)
set(borrowed_DIST
    ${borrowed_DIST_local}
    ${chartjs-2_DIST}
    ${chartjs-4_DIST}
    ${d3-3_DIST}
    ${libc_DIST}
    ${guile-json_DIST}
    ${goffice_DIST}
    ${d3-sankey_DIST}
    PARENT_SCOPE)
