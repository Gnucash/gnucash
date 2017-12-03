
INCLUDE_DIRECTORIES(
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
)

ADD_LIBRARY(gncmod_agedver EXCLUDE_FROM_ALL agedver.c)
TARGET_LINK_LIBRARIES(gncmod_agedver ${GLIB2_LDFLAGS})

ADD_LIBRARY(gncmod_futuremodsys  EXCLUDE_FROM_ALL futuremodsys.c)
TARGET_LINK_LIBRARIES(gncmod_futuremodsys ${GLIB2_LDFLAGS})

ADD_LIBRARY(gncmod_incompatdep  EXCLUDE_FROM_ALL incompatdep.c)
TARGET_LINK_LIBRARIES(gncmod_incompatdep gnc-module ${GLIB2_LDFLAGS})

SET_TARGET_PROPERTIES(gncmod_agedver gncmod_futuremodsys gncmod_incompatdep PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

SET_DIST_LIST(misc_mods_DIST CMakeLists.txt agedver.c futuremodsys.c incompatdep.c)
