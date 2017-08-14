
ADD_SUBDIRECTORY(xml)
IF (WITH_SQL)
  ADD_SUBDIRECTORY (dbi)
  ADD_SUBDIRECTORY (sql)
ENDIF (WITH_SQL)



SET_LOCAL_DIST(backend_DIST_local CMakeLists.txt Makefile.am)
SET(backend_DIST ${backend_DIST_local} ${backend_dbi_DIST} ${backend_sql_DIST} ${backend_xml_DIST} PARENT_SCOPE)