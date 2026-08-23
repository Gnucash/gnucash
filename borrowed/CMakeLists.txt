add_subdirectory(libc)
add_subdirectory(goffice)
add_subdirectory(chartjs-4)
add_subdirectory(guile-json)
add_subdirectory(ctre)

set_local_dist(borrowed_DIST_local jenny/jenny.c CMakeLists.txt README)
set(borrowed_DIST
    ${borrowed_DIST_local}
    ${chartjs-4_DIST}
    ${libc_DIST}
    ${guile-json_DIST}
    ${goffice_DIST}
    ${ctre_DIST}
    PARENT_SCOPE)
