add_subdirectory(us)

set_local_dist(locale_specific_DIST_local CMakeLists.txt )
set(locale_specific_DIST ${locale_specific_DIST_local} ${locale_specific_us_DIST} PARENT_SCOPE)