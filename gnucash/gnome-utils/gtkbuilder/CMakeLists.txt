FILE(COPY .
  DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder
  PATTERN Makefile.* EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION share/gnucash
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE)

SET(gnome_utils_GLADE
        assistant-xml-encoding.glade
        dialog-account.glade
        dialog-book-close.glade
        dialog-commodity.glade
        dialog-file-access.glade
        dialog-object-references.glade
        dialog-options.glade
        dialog-preferences.glade
        dialog-query-view.glade
        dialog-reset-warnings.glade
        dialog-tax-table.glade
        dialog-totd.glade
        dialog-transfer.glade
        dialog-userpass.glade
        gnc-date-format.glade
        gnc-frequency.glade
        gnc-recurrence.glade
        gnc-tree-view-owner.glade
)

SET_DIST_LIST(gnome_utils_gtkbuilder_DIST CMakeLists.txt ${gnome_utils_GLADE})