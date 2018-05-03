set (ui_SOURCES
      gnc-main-window-ui.xml
      gnc-plugin-account-tree-ui.xml
      gnc-plugin-basic-commands-ui.xml
      gnc-plugin-bi-import-ui.xml
      gnc-plugin-budget-ui.xml
      gnc-plugin-business-ui.xml
      gnc-plugin-csv-export-ui.xml
      gnc-plugin-csv-import-ui.xml
      gnc-plugin-customer-import-ui.xml
      gnc-plugin-file-history-ui.xml
      gnc-plugin-log-replay-ui.xml
      gnc-plugin-page-account-tree2-ui.xml
      gnc-plugin-page-account-tree-ui.xml
      gnc-plugin-page-budget-ui.xml
      gnc-plugin-page-invoice-ui.xml
      gnc-plugin-page-owner-tree-ui.xml
      gnc-plugin-page-register2-ui.xml
      gnc-plugin-page-register-ui.xml
      gnc-plugin-page-report-ui.xml
      gnc-plugin-page-sx-list2-ui.xml
      gnc-plugin-page-sx-list-ui.xml
      gnc-plugin-page-sxregister-ui.xml
      gnc-plugin-qif-import-ui.xml
      gnc-plugin-register22-ui.xml
      gnc-plugin-register2-ui.xml
      gnc-plugin-register-ui.xml
      gnc-plugin-stylesheets-ui.xml
      gnc-reconcile-window-ui.xml
      gnc-sxed-to-create-window-ui.xml
      gnc-sxed-window-ui-full.xml
      gnc-sxed-window-ui.xml
      gnc-windows-menu-ui-quartz.xml
      gnc-windows-menu-ui.xml
      osx_accel_map)

file (COPY ${ui_SOURCES} DESTINATION ${DATADIR_BUILD}/gnucash/ui)

install (FILES ${ui_SOURCES} DESTINATION share/gnucash/ui)

set_dist_list(ui_DIST CMakeLists.txt ${ui_SOURCES})
