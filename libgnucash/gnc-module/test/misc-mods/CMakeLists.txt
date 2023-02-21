
INCLUDE_DIRECTORIES(
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
)

add_library(gncmod-agedver EXCLUDE_FROM_ALL agedver.c)
target_link_libraries(gncmod-agedver PkgConfig::GLIB2)

add_library(gncmod-futuremodsys  EXCLUDE_FROM_ALL futuremodsys.c)
target_link_libraries(gncmod-futuremodsys PkgConfig::GLIB2)

add_library(gncmod-incompatdep  EXCLUDE_FROM_ALL incompatdep.c)
target_link_libraries(gncmod-incompatdep gnc-module PkgConfig::GLIB2)

set_target_properties(gncmod-agedver gncmod-incompatdep PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/test)

set_target_properties(gncmod-futuremodsys PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test/future
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test/future
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/test/future)

set_dist_list(misc_mods_DIST CMakeLists.txt agedver.c futuremodsys.c incompatdep.c)
