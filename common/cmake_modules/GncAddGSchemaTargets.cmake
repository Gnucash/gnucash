macro(add_gschema_targets _gschema_INPUTS)
  SET(_gschema_OUTPUTS "")
  set(local_depends ${gschema_depends})
  # FIXME: I have no idea of I'm using the right options here for intltool-merge for Windows.
  SET(INITTOOL_OPTIONS "--no-translations")
  IF(WIN32)
    SET(INITTOOL_OPTIONS "/tmp")
  ENDIF(WIN32)
  SET(CMAKE_COMMAND_TMP "")
  IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
    SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
  ENDIF()
  FOREACH(file ${_gschema_INPUTS})
    configure_file(${file}.in.in ${file}.in @ONLY)

    set(_OUTPUT_FILE ${DATADIR_BUILD}/glib-2.0/schemas/${file})
    ADD_CUSTOM_COMMAND(
        OUTPUT ${_OUTPUT_FILE}
        COMMAND ${CMAKE_COMMAND_TMP}
          LC_ALL=C
          ${PERL_EXECUTABLE} ${INTLTOOL_MERGE} -x -u ${INITTOOL_OPTIONS} ${CMAKE_CURRENT_BINARY_DIR}/${file}.in ${_OUTPUT_FILE}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.in
        MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${file}.in.in
    )
    list(APPEND _gschema_OUTPUTS ${_OUTPUT_FILE})

    string(REPLACE ".xml" ".valid" file_no_xml ${file})
    set(_VALID_FILE ${CMAKE_CURRENT_BINARY_DIR}/${file_no_xml})
    list(APPEND _gschema_VALIDS ${_VALID_FILE})
    ADD_CUSTOM_COMMAND(
        OUTPUT ${_VALID_FILE}
        COMMAND ${CMAKE_COMMAND_TMP}
          ${GLIB_COMPILE_SCHEMAS} --strict --dry-run --schema-file=${_OUTPUT_FILE}
        COMMAND ${CMAKE_COMMAND} -E touch ${_VALID_FILE}
        DEPENDS ${_OUTPUT_FILE}
    )
    add_custom_target(${file_no_xml}-target DEPENDS ${_VALID_FILE})

    # Add both a target and a file level dependency for the valid target/file
    # to the gschemas.compiled target (local_depends will be propagated to that target)
    # The target level one is to ensure a link between two targets in different directories (required for the make build chain)
    # The file level one is to ensure gschemas.compiled will be rebuilt if any of the
    # dependencies changes.
    list(APPEND local_depends ${file_no_xml}-target ${_VALID_FILE})
  ENDFOREACH(file)

  set(gschema_depends ${local_depends} CACHE INTERNAL "gschemas.compiled dependencies")

  INSTALL(FILES ${_gschema_OUTPUTS} DESTINATION share/glib-2.0/schemas)

endmacro()
