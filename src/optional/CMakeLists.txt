ADD_SUBDIRECTORY(python-bindings)

SET_LOCAL_DIST(optional_DIST_local CMakeLists.txt Makefile.am)
SET(optional_DIST ${optional_DIST_local} ${python_bindings_DIST} PARENT_SCOPE)
