
INCLUDE_DIRECTORIES(
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GLIB2_INCLUDE_DIRS}
)

add_library(gncmod-agedver EXCLUDE_FROM_ALL agedver.c)
target_link_libraries(gncmod-agedver ${GLIB2_LDFLAGS})

add_library(gncmod-futuremodsys  EXCLUDE_FROM_ALL futuremodsys.c)
target_link_libraries(gncmod-futuremodsys ${GLIB2_LDFLAGS})

add_library(gncmod-incompatdep  EXCLUDE_FROM_ALL incompatdep.c)
target_link_libraries(gncmod-incompatdep gnc-module ${GLIB2_LDFLAGS})

set_target_properties(gncmod-agedver gncmod-futuremodsys gncmod-incompatdep PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

set_dist_list(misc_mods_DIST CMakeLists.txt agedver.c futuremodsys.c incompatdep.c)
