set (ui_SOURCES
      gnc-reconcile-window-ui.xml
      osx_accel_map)

foreach (ui_file ${ui_SOURCES})
    configure_file (${ui_file} ${DATADIR_BUILD}/gnucash/ui/${ui_file} COPYONLY)
endforeach()

install (FILES ${ui_SOURCES} DESTINATION share/gnucash/ui)

set_dist_list(ui_DIST CMakeLists.txt ${ui_SOURCES})
