FILE(COPY .
        DESTINATION ${DATADIR_BUILD}/gnucash/ui
        PATTERN Makefile.* EXCLUDE
        PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION share/gnucash
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE)

SET(gnome_UI
      gnc-plugin-account-tree-ui.xml
      gnc-plugin-basic-commands-ui.xml
      gnc-plugin-budget-ui.xml
      gnc-plugin-business-ui.xml
      gnc-plugin-file-history-ui.xml
      gnc-plugin-page-account-tree2-ui.xml
      gnc-plugin-page-account-tree-ui.xml
      gnc-plugin-page-budget-ui.xml
      gnc-plugin-page-invoice-ui.xml
      gnc-plugin-page-owner-tree-ui.xml
      gnc-plugin-page-register2-ui.xml
      gnc-plugin-page-register-ui.xml
      gnc-plugin-page-sx-list2-ui.xml
      gnc-plugin-page-sx-list-ui.xml
      gnc-plugin-page-sxregister-ui.xml
      gnc-plugin-register22-ui.xml
      gnc-plugin-register2-ui.xml
      gnc-plugin-register-ui.xml
      gnc-reconcile-window-ui.xml
      gnc-sxed-to-create-window-ui.xml
      gnc-sxed-window-ui-full.xml
      gnc-sxed-window-ui.xml)

SET_DIST_LIST(gnome_ui_DIST CMakeLists.txt ${gnome_UI})


