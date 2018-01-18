# CMakeLists.txt for libgnucash/

# The subdirectories
ADD_SUBDIRECTORY (app-utils)
ADD_SUBDIRECTORY (backend)
ADD_SUBDIRECTORY (core-utils)
ADD_SUBDIRECTORY (doc)
ADD_SUBDIRECTORY (engine)
ADD_SUBDIRECTORY (gnc-module)
ADD_SUBDIRECTORY (quotes)
ADD_SUBDIRECTORY (scm)
ADD_SUBDIRECTORY (tax)

ADD_DEFINITIONS (-DHAVE_CONFIG_H)

SET_LOCAL_DIST(libgnucash_DIST_local CMakeLists.txt ${libgnucash_EXTRA_DIST})

SET(libgnucash_DIST ${libgnucash_DIST_local} ${app_utils_DIST} ${backend_DIST}
             ${core_utils_DIST} ${doc_DIST} ${engine_DIST} ${gnc_module_DIST}
             ${quotes_DIST} ${scm_DIST} ${tax_DIST} PARENT_SCOPE)
