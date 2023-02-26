
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

set(local_preftrans_files ${gschema_preftrans_files})
list(APPEND local_preftrans_files ${CMAKE_CURRENT_SOURCE_DIR}/pref_transformations.xml)
set(gschema_preftrans_files ${local_preftrans_files} CACHE INTERNAL "files describing transformations for our gsettings based preferences")

# Provide gnucash runtime with a list of potential transformations to user set preferences
# following GSettings schema changes between gnucash releases
add_custom_command(
    OUTPUT ${DATADIR_BUILD}/${PROJECT_NAME}/pref_transformations.xml
    COMMAND env cat ${gschema_preftrans_files} > ${DATADIR_BUILD}/${PROJECT_NAME}/pref_transformations.xml
    DEPENDS ${gschema_preftrans_files}
)

add_custom_target(pref_transformations ALL DEPENDS ${DATADIR_BUILD}/${PROJECT_NAME}/pref_transformations.xml)

install(FILES ${DATADIR_BUILD}/${PROJECT_NAME}/pref_transformations.xml
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})

# Handle gschemas.compiled
if (COMPILE_GSCHEMAS)

    add_custom_command(
        OUTPUT ${SCHEMADIR_BUILD}/gschemas.compiled
        COMMAND ${GLIB_COMPILE_SCHEMAS} --strict ${SCHEMADIR_BUILD}
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
                              ${GLIB_COMPILE_SCHEMAS} --strict \$ENV\{DESTDIR\}${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas\")")
endif()

set(gschemas_DIST_local pref_transformations.xml)
foreach(file ${gschema_SOURCES})
    list(APPEND gschemas_DIST_local ${file}.in)
endforeach()
set_dist_list(gschemas_DIST CMakeLists.txt ${gschemas_DIST_local})
