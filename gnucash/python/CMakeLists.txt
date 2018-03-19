SET(pycons_DATA
  pycons/__init__.py
  pycons/console.py
  pycons/ishell.py
  pycons/pycons
  pycons/setup.py
  pycons/shell.py
  pycons/simple_plot.py
)

IF (WITH_PYTHON)
  INSTALL(FILES ${pycons_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python/pycons)
  INSTALL(FILES init.py DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python)

  FILE(COPY ${pycons_DATA} DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python/pycons)
  FILE(COPY init.py DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python)
ENDIF(WITH_PYTHON)

SET_LOCAL_DIST(pycons_DIST ${pycons_DATA})
SET_LOCAL_DIST(python_DIST_local CMakeLists.txt init.py)
SET(python_DIST ${python_DIST_local} ${pycons_DIST} PARENT_SCOPE)

