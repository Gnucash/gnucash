# Post install actions go here.

IF (APPLE)
  INSTALL(CODE "EXECUTE_PROCESS(
                COMMAND /usr/bin/install_name_tool -add_rpath ${CMAKE_INSTALL_LIBDIR}
                                                   -add_rpath ${PKGLIBDIR}
                                                   ${CMAKE_INSTALL_BINDIR}/gnucash)")
ENDIF(APPLE)


SET_DIST_LIST(cmake_DIST CMakeLists.txt README_CMAKE.txt cmake_uninstall.cmake.in)
