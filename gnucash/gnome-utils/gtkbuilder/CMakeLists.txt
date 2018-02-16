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

FILE(COPY ${gnome_utils_GLADE} DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

INSTALL(FILES ${gnome_utils_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET_DIST_LIST(gnome_utils_gtkbuilder_DIST CMakeLists.txt ${gnome_utils_GLADE})
