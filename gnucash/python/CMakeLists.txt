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
  install(FILES ${pycons_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python/pycons)
  install(FILES init.py DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python)

  file(COPY ${pycons_DATA} DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python/pycons)
  file(COPY init.py DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python)
endif(WITH_PYTHON)

set_local_dist(pycons_DIST ${pycons_DATA})
set_local_dist(python_DIST_local CMakeLists.txt init.py)
set(python_DIST ${python_DIST_local} ${pycons_DIST} PARENT_SCOPE)

