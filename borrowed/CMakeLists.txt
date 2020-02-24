add_subdirectory(libc)
add_subdirectory(goffice)
add_subdirectory(chartjs)
add_subdirectory(guile-json)

set_local_dist(borrowed_DIST_local jenny/jenny.c CMakeLists.txt README)
set(borrowed_DIST ${borrowed_DIST_local} ${chartjs_DIST} ${libc_DIST} ${guile-json_DIST} ${goffice_DIST} PARENT_SCOPE)
