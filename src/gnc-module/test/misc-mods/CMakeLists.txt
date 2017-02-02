
INCLUDE_DIRECTORIES(
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${GLIB2_INCLUDE_DIRS}
)

ADD_LIBRARY(gncmod_agedver EXCLUDE_FROM_ALL agedver.c)
TARGET_LINK_LIBRARIES(gncmod_agedver ${GLIB2_LDFLAGS})

ADD_LIBRARY(gncmod_futuremodsys  EXCLUDE_FROM_ALL futuremodsys.c)
TARGET_LINK_LIBRARIES(gncmod_futuremodsys ${GLIB2_LDFLAGS})

ADD_LIBRARY(gncmod_incompatdep  EXCLUDE_FROM_ALL incompatdep.c)
TARGET_LINK_LIBRARIES(gncmod_incompatdep gnc-module ${GLIB2_LDFLAGS})

SET_DIST_LIST(misc_mods_DIST CMakeLists.txt Makefile.am agedver.c futuremodsys.c incompatdep.c)