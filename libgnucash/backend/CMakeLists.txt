
add_subdirectory(xml)
add_subdirectory (dbi)
add_subdirectory (sql)



set_local_dist(backend_DIST_local CMakeLists.txt )
set(backend_DIST ${backend_DIST_local} ${backend_dbi_DIST} ${backend_sql_DIST} ${backend_xml_DIST} PARENT_SCOPE)