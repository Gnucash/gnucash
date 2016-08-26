# handle pycons directory

SET(gncmod_python_SOURCES gncmod-python.c)

ADD_LIBRARY(gncmod-python ${gncmod_python_SOURCES})
TARGET_LINK_LIBRARIES(gncmod-python gnc-module gnc-core-utils-python gncmod-app-utils-python
   ${PYTHON_LIBRARIES} ${GLIB_LIBS})   # ${PYTHON_EXTRA_LIBS}
TARGET_INCLUDE_DIRECTORIES(gncmod-python
  PRIVATE ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/core_utils ${CMAKE_SOURCE_DIR}/gnc-module ${PYTHON_INCLUDE_DIR})
TARGET_COMPILE_OPTIONS(gncmod-python PRIVATE -DG_LOG_DOMAIN=\"gnc.python\")
INSTALL(TARGETS gncmod-python
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin
)

INSTALL(FILES init.py DESTINATION share/python)