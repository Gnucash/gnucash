
add_subdirectory(xml2)

set_local_dist(test_backend_xml_test_files_DIST_local CMakeLists.txt )
set(test_backend_xml_test_files_DIST ${test_backend_xml_test_files_DIST_local} ${xml2_DIST} PARENT_SCOPE)

