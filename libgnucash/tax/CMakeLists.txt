
add_subdirectory(us)

set_local_dist(tax_DIST_local CMakeLists.txt )
set(tax_DIST ${tax_DIST_local} ${locale_tax_DIST} PARENT_SCOPE)
