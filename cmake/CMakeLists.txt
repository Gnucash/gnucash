# Post install actions go here.

INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${GLIB_COMPILE_SCHEMAS} ${CMAKE_INSTALL_PREFIX}/share/glib-2.0/schemas)")

IF (APPLE)
  INSTALL(CODE "EXECUTE_PROCESS(
                COMMAND /usr/bin/install_name_tool -add_rpath ${CMAKE_INSTALL_PREFIX}/lib
                                                   -add_rpath ${CMAKE_INSTALL_PREFIX}/lib/gnucash
                                                   ${CMAKE_INSTALL_PREFIX}/bin/gnucash)")
ENDIF(APPLE)


IF (GNC_BUILD_AS_INSTALL)
  SET(schema-targets aqb-gschema business-gnome-gschema csv-exp-gschema csv-imp-gschema
      generic-import-gschema gnome-gschema gnome-utils-gschema ofx-gschema qif-imp-gschema)

  SET(SCHEMA_DIRECTORY ${DATADIR_BUILD}/glib-2.0/schemas)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${SCHEMA_DIRECTORY}/gschemas.compiled
    COMMAND ${CMAKE_COMMAND} -E env ${GLIB_COMPILE_SCHEMAS} ${SCHEMA_DIRECTORY}
    DEPENDS ${schema-targets}
  )

  ADD_CUSTOM_TARGET(compiled-schemas ALL DEPENDS ${SCHEMA_DIRECTORY}/gschemas.compiled)

ENDIF(GNC_BUILD_AS_INSTALL)

INSTALL(FILES INSTALL COPYING DESTINATION share/doc/gnucash)