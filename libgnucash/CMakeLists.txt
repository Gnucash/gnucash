# CMakeLists.txt for libgnucash/

# The subdirectories
add_subdirectory (app-utils)
add_subdirectory (backend)
add_subdirectory (core-utils)
add_subdirectory (doc)
add_subdirectory (engine)
add_subdirectory (gnc-module)
add_subdirectory (quotes)
add_subdirectory (scm)
add_subdirectory (tax)

add_definitions (-DHAVE_CONFIG_H)

set_local_dist(libgnucash_DIST_local CMakeLists.txt ${libgnucash_EXTRA_DIST})

set(libgnucash_DIST ${libgnucash_DIST_local} ${app_utils_DIST} ${backend_DIST}
             ${core_utils_DIST} ${doc_DIST} ${engine_DIST} ${gnc_module_DIST}
             ${quotes_DIST} ${scm_DIST} ${tax_DIST} PARENT_SCOPE)
