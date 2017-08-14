ADD_SUBDIRECTORY(valgrind)

SET_LOCAL_DIST(debug_DIST_local CMakeLists.txt Makefile.am)
SET(debug_DIST ${debug_DIST_local} ${valgrind_DIST} PARENT_SCOPE)