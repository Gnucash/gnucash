
INCLUDE_DIRECTORIES(
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
)

ADD_LIBRARY(gncmod-agedver EXCLUDE_FROM_ALL agedver.c)
TARGET_LINK_LIBRARIES(gncmod-agedver ${GLIB2_LDFLAGS})

ADD_LIBRARY(gncmod-futuremodsys  EXCLUDE_FROM_ALL futuremodsys.c)
TARGET_LINK_LIBRARIES(gncmod-futuremodsys ${GLIB2_LDFLAGS})

ADD_LIBRARY(gncmod-incompatdep  EXCLUDE_FROM_ALL incompatdep.c)
TARGET_LINK_LIBRARIES(gncmod-incompatdep gnc-module ${GLIB2_LDFLAGS})

SET_TARGET_PROPERTIES(gncmod-agedver gncmod-futuremodsys gncmod-incompatdep PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

SET_DIST_LIST(misc_mods_DIST CMakeLists.txt agedver.c futuremodsys.c incompatdep.c)
