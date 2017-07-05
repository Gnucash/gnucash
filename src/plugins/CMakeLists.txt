ADD_SUBDIRECTORY(bi_import)
ADD_SUBDIRECTORY(customer_import)
ADD_SUBDIRECTORY(example)


SET_LOCAL_DIST(plugins_DIST_local CMakeLists.txt Makefile.am)
SET(plugins_DIST ${plugins_DIST_local} ${bi_import_DIST} ${customer_import_DIST} ${example_DIST} PARENT_SCOPE)