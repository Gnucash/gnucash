# Post install actions go here.

if (APPLE)
  install(CODE "execute_process(
                COMMAND /usr/bin/install_name_tool -add_rpath ${CMAKE_INSTALL_LIBDIR}
                                                   -add_rpath ${PKGLIBDIR}
                                                   ${CMAKE_INSTALL_BINDIR}/gnucash)")
endif()


set_dist_list(cmake_DIST CMakeLists.txt README_CMAKE.txt cmake_uninstall.cmake.in
    configure-appdata.cmake configure-gnucash-desktop.cmake configure-manpage.cmake
    git2version-info.cmake version-info2env.cmake
)
