# CMakeLists.txt for data/

# The subdirectories
ADD_SUBDIRECTORY (accounts)
ADD_SUBDIRECTORY (checks)
ADD_SUBDIRECTORY (pixmaps)

SET_LOCAL_DIST(data_DIST_local CMakeLists.txt ${data_EXTRA_DIST})

SET(data_DIST ${data_DIST_local} ${accounts_DIST} ${checks_DIST}
             ${pixmaps_DIST} PARENT_SCOPE)
