FILE(COPY .
  DESTINATION ${DATADIR_BUILD}/gnucash/ui
  PATTERN Makefile.* EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION share/gnucash
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE)

SET(gnome_utils_ui_DIST_local
        CMakeLists.txt
        gnc-main-window-ui.xml
        gnc-windows-menu-ui-quartz.xml
        gnc-windows-menu-ui.xml
        
        osx_accel_map
        )

SET_DIST_LIST(gnome_utils_ui_DIST ${gnome_utils_ui_DIST_local})
