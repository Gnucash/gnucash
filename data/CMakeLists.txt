# CMakeLists.txt for data/

# The subdirectories
add_subdirectory (accounts)
add_subdirectory (checks)
add_subdirectory (pixmaps)

set_local_dist(data_DIST_local CMakeLists.txt ${data_EXTRA_DIST})

set(data_DIST ${data_DIST_local} ${accounts_DIST} ${checks_DIST}
             ${pixmaps_DIST} PARENT_SCOPE)
