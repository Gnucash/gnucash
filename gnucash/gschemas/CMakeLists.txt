
SET(gschema_SOURCES
  org.gnucash.dialogs.business.gschema.xml
  org.gnucash.dialogs.checkprinting.gschema.xml
  org.gnucash.dialogs.commodities.gschema.xml
  org.gnucash.dialogs.export.csv.gschema.xml
  org.gnucash.dialogs.gschema.xml
  org.gnucash.dialogs.import.csv.gschema.xml
  org.gnucash.dialogs.import.generic.gschema.xml
  org.gnucash.dialogs.import.qif.gschema.xml
  org.gnucash.dialogs.reconcile.gschema.xml
  org.gnucash.dialogs.sxs.gschema.xml
  org.gnucash.dialogs.totd.gschema.xml
  org.gnucash.general.finance-quote.gschema.xml
  org.gnucash.gschema.xml
  org.gnucash.history.gschema.xml
  org.gnucash.warnings.gschema.xml
  org.gnucash.window.pages.account.tree.gschema.xml
  org.gnucash.window.pages.gschema.xml
)

add_gschema_targets("${gschema_SOURCES}")

# Handle gschemas.compiled
if (COMPILE_GSCHEMAS)
    set(CMAKE_COMMAND_TMP "")
    if (${CMAKE_VERSION} VERSION_GREATER 3.1)
        set(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
    endif()

    add_custom_command(
        OUTPUT ${SCHEMADIR_BUILD}/gschemas.compiled
        COMMAND ${CMAKE_COMMAND_TMP} ${GLIB_COMPILE_SCHEMAS} ${SCHEMADIR_BUILD}
        DEPENDS ${gschema_depends}
    )

    add_custom_target(compiled-schemas ALL DEPENDS ${SCHEMADIR_BUILD}/gschemas.compiled)


    install(CODE "execute_process(
        COMMAND ${SHELL} -c \"echo Compiling gschema files in $DESTDIR${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas ;
                              ${GLIB_COMPILE_SCHEMAS} $DESTDIR${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas\")")
endif ()

SET(gschemas_DIST_local "")
FOREACH(file ${gschema_SOURCES})
    LIST(APPEND gschemas_DIST_local ${file}.in)
ENDFOREACH()
SET_DIST_LIST(gschemas_DIST CMakeLists.txt ${gschemas_DIST_local})
