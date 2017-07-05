ADD_SUBDIRECTORY(libc)
ADD_SUBDIRECTORY(goffice)

SET_LOCAL_DIST(lib_DIST_local CMakeLists.txt Makefile.am README)
SET(lib_DIST ${lib_DIST_local} ${libc_DIST} ${goffice_DIST} PARENT_SCOPE)
