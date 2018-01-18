ADD_SUBDIRECTORY(us)

SET_LOCAL_DIST(locale_specific_DIST_local CMakeLists.txt )
SET(locale_specific_DIST ${locale_specific_DIST_local} ${locale_specific_us_DIST} PARENT_SCOPE)