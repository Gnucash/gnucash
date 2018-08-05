add_subdirectory(glade)
add_subdirectory(ui)

set(example_SOURCES gnc-plugin.example.c gncmod-example.c)
set(example_noinst_HEADERS gnc-plugin.example.h)

set_local_dist(example_DIST_local CMakeLists.txt ${example_SOURCES} ${example_noinst_HEADERS})
set(example_DIST ${example_DIST_local} ${example_glade_DIST} ${example_ui_DIST} PARENT_SCOPE)