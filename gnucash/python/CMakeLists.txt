set(gncmod_python_SOURCES gncmod-python.c)

set(pycons_DATA
  pycons/__init__.py
  pycons/console.py
  pycons/ishell.py
  pycons/pycons
  pycons/setup.py
  pycons/shell.py
  pycons/simple_plot.py
)

if (WITH_PYTHON)

  add_library(gncmod-python ${gncmod_python_SOURCES})
  # target_link_libraries(gncmod-python gnc-module gnc-core-utils-python gncmod-app-utils-python
  target_link_libraries(gncmod-python gnc-module gnc-core-utils
   ${PYTHON_LIBRARIES} ${GLIB_LIBS})   # ${PYTHON_EXTRA_LIBS}
  target_include_directories(gncmod-python
    PRIVATE ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/libgnucash/core-utils ${CMAKE_SOURCE_DIR}/gnc-module ${PYTHON_INCLUDE_DIR})
  target_compile_options(gncmod-python PRIVATE -DG_LOG_DOMAIN=\"gnc.python\")
if (APPLE)
  set_target_properties (gncmod-python PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()
  install(TARGETS gncmod-python
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )

  install(FILES ${pycons_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python/pycons)
  install(FILES init.py DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python)

  file(COPY ${pycons_DATA} DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python/pycons)
  file(COPY init.py DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python)
endif()

set_local_dist(pycons_DIST ${pycons_DATA})
set_local_dist(python_DIST_local CMakeLists.txt gncmod-python.c init.py)
set(python_DIST ${python_DIST_local} ${pycons_DIST} PARENT_SCOPE)

