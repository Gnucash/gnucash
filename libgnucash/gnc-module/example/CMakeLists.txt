ADD_SUBDIRECTORY(glade)
ADD_SUBDIRECTORY(ui)

SET(example_SOURCES gnc-plugin.example.c gncmod-example.c)
SET(example_noinst_HEADERS gnc-plugin.example.h)

SET_LOCAL_DIST(example_DIST_local CMakeLists.txt ${example_SOURCES} ${example_noinst_HEADERS})
SET(example_DIST ${example_DIST_local} ${example_glade_DIST} ${example_ui_DIST} PARENT_SCOPE)