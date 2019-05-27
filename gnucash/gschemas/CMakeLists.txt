
set(gschema_SOURCES
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

    add_custom_command(
        OUTPUT ${SCHEMADIR_BUILD}/gschemas.compiled
        COMMAND ${CMAKE_COMMAND} -E env ${GLIB_COMPILE_SCHEMAS} ${SCHEMADIR_BUILD}
        DEPENDS ${gschema_depends}
    )

    add_custom_target(compiled-schemas ALL DEPENDS ${SCHEMADIR_BUILD}/gschemas.compiled)


    install(CODE "execute_process(
        COMMAND ${SHELL} -c \"echo Compiling gschema files in $DESTDIR${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas ;
                              ${GLIB_COMPILE_SCHEMAS} $DESTDIR${CMAKE_INSTALL_FULL_DATADIR}/glib-2.0/schemas\")")
endif ()

set(gschemas_DIST_local "")
foreach(file ${gschema_SOURCES})
    list(APPEND gschemas_DIST_local ${file}.in)
endforeach()
set_dist_list(gschemas_DIST CMakeLists.txt ${gschemas_DIST_local})
