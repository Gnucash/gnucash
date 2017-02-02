ADD_SUBDIRECTORY(libc)
ADD_SUBDIRECTORY(stf)

SET_LOCAL_DIST(lib_DIST_local CMakeLists.txt Makefile.am README)
SET(lib_DIST ${lib_DIST_local} ${libc_DIST} ${stf_DIST} PARENT_SCOPE)