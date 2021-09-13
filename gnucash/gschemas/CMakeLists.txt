
set(gschema_SOURCES
  org.gnucash.GnuCash.deprecated.gschema.xml
  org.gnucash.GnuCash.dialogs.business.gschema.xml
  org.gnucash.GnuCash.dialogs.checkprinting.gschema.xml
  org.gnucash.GnuCash.dialogs.commodities.gschema.xml
  org.gnucash.GnuCash.dialogs.export.csv.gschema.xml
  org.gnucash.GnuCash.dialogs.gschema.xml
  org.gnucash.GnuCash.dialogs.import.csv.gschema.xml
  org.gnucash.GnuCash.dialogs.import.generic.gschema.xml
  org.gnucash.GnuCash.dialogs.import.qif.gschema.xml
  org.gnucash.GnuCash.dialogs.reconcile.gschema.xml
  org.gnucash.GnuCash.dialogs.sxs.gschema.xml
  org.gnucash.GnuCash.dialogs.totd.gschema.xml
  org.gnucash.GnuCash.general.finance-quote.gschema.xml
  org.gnucash.GnuCash.gschema.xml
  org.gnucash.GnuCash.history.gschema.xml
  org.gnucash.GnuCash.warnings.gschema.xml
  org.gnucash.GnuCash.window.pages.account.tree.gschema.xml
  org.gnucash.GnuCash.window.pages.gschema.xml
)

add_gschema_targets("${gschema_SOURCES}")

# Provide gnucash runtime with a list of migratable preferences
configure_file(migratable-prefs.xml ${DATADIR_BUILD}/${PROJECT_NAME}/migratable-prefs.xml COPYONLY)
install(FILES ${DATADIR_BUILD}/${PROJECT_NAME}/migratable-prefs.xml
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})

# Handle gschemas.compiled
if (COMPILE_GSCHEMAS)

    add_custom_command(
        OUTPUT ${SCHEMADIR_BUILD}/gschemas.compiled
        COMMAND ${CMAKE_COMMAND} -E env ${GLIB_COMPILE_SCHEMAS} ${SCHEMADIR_BUILD}
        DEPENDS ${gschema_depends}
    )

    add_custom_target(compiled-schemas ALL DEPENDS ${SCHEMADIR_BUILD}/gschemas.compiled)

    # On Windows concatenating two absolute paths results in an invalid path (having two drive letters)
    # If DESTDIR is not set on the other hand, the below install command works just fine
    # So verify DESTDIR is not set on Windows
    # Note we have to do this at build time, not configure time so the guard is part of the custom install command
    install(CODE "
    if (WIN32)
        set (DESTDIR \$ENV\{DESTDIR\})
        if (DESTDIR)
            message(SEND_ERROR \"GnuCash can't be built with the DESTDIR environment variable set on Windows (due to bad interference with glib-compile-schemas).\")
        endif()
    endif()
    execute_process(
        COMMAND ${SHELL} -c \"echo Compiling gschema files in \$ENV\{DESTDIR\}${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas ;
                              ${GLIB_COMPILE_SCHEMAS} \$ENV\{DESTDIR\}${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas\")")
endif()

set(gschemas_DIST_local migratable-prefs.xml)
foreach(file ${gschema_SOURCES})
    list(APPEND gschemas_DIST_local ${file}.in)
endforeach()
set_dist_list(gschemas_DIST CMakeLists.txt ${gschemas_DIST_local})
