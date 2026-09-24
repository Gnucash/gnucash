# Post install actions go here.

if (APPLE)
  install(CODE "execute_process(
                COMMAND /usr/bin/install_name_tool -add_rpath ${CMAKE_INSTALL_LIBDIR}
                                                   -add_rpath ${PKGLIBDIR}
                                                   ${CMAKE_INSTALL_BINDIR}/gnucash)")
endif()

add_test(NAME check-cmake-dist-manifest
  COMMAND ${CMAKE_COMMAND}
    -DDIST_MANIFEST=${CMAKE_BINARY_DIR}/dist_manifest.txt
    -P ${CMAKE_CURRENT_SOURCE_DIR}/check-cmake-dist-manifest.cmake)

add_test(NAME check-gapplication-command-line-forwarding
  COMMAND ${CMAKE_COMMAND}
    -DSOURCE_ROOT=${CMAKE_SOURCE_DIR}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/check-gapplication-command-line-forwarding.cmake)


set_dist_list(cmake_DIST
  CMakeLists.txt
  README_CMAKE.txt
  cmake_uninstall.cmake.in
  check-cmake-dist-manifest.cmake
  check-gapplication-command-line-forwarding.cmake
  check-gnc-vcs-info-worktree.cmake
  check-gtk-window-application-bindings.cmake
  check-no-local-main-loop-pumps.cmake
  configure-appdata.cmake
  configure-gnucash-desktop.cmake
  configure-manpage.cmake
  configure-totd.cmake
  git2version-info.cmake
  no-vcs2version-info.cmake
  run-ctest.cmake
  version-info2env.cmake
)
