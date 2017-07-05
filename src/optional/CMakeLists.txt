ADD_SUBDIRECTORY(python-bindings)
ADD_SUBDIRECTORY(gtkmm)

SET_LOCAL_DIST(optional_DIST_local CMakeLists.txt Makefile.am)
SET(optional_DIST ${optional_DIST_local} ${gtkmm_DIST} ${python_bindings_DIST} PARENT_SCOPE)