set(gnome_utils_UI
  gnc-main-window-ui.xml
  gnc-windows-menu-ui-quartz.xml
  gnc-windows-menu-ui.xml
  osx_accel_map
  )

FILE(COPY ${gnome_utils_UI} DESTINATION ${DATADIR_BUILD}/gnucash/ui)

INSTALL(FILES ${gnome_utils_UI} DESTINATION share/gnucash/ui)

SET(gnome_utils_ui_DIST_local
  CMakeLists.txt
  ${gnome_utils_UI}
  )

SET_DIST_LIST(gnome_utils_ui_DIST ${gnome_utils_ui_DIST_local})
