
add_library(withdep EXCLUDE_FROM_ALL withdep.c withdep.h)
target_include_directories(withdep PRIVATE
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module/test/mod-ordinary
)
target_link_libraries(withdep ordinary)


add_library(gncmod-withdep EXCLUDE_FROM_ALL gnc-mod-withdep.c)

target_link_libraries(gncmod-withdep withdep gnc-module)
target_include_directories(gncmod-withdep PRIVATE
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/common
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${GUILE_INCLUDE_DIRS}
)

set_target_properties(withdep gncmod-withdep PROPERTIES
LIBRARY_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
ARCHIVE_OUTPUT_DIRECTORY ${LIBDIR_BUILD}/gnucash/test
RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/test)

set_dist_list(mod_withdep_DIST CMakeLists.txt withdep.c withdep.h gnc-mod-withdep.c)
