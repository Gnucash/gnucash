
ADD_SUBDIRECTORY(xml2)

SET_LOCAL_DIST(test_backend_xml_test_files_DIST_local CMakeLists.txt )
SET(test_backend_xml_test_files_DIST ${test_backend_xml_test_files_DIST_local} ${xml2_DIST} PARENT_SCOPE)

