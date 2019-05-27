macro(add_gschema_targets _gschema_INPUTS)
  set(_gschema_OUTPUTS "")
  set(local_depends ${gschema_depends})
  foreach(file ${_gschema_INPUTS})

    set(_OUTPUT_FILE ${DATADIR_BUILD}/glib-2.0/schemas/${file})
    configure_file(${file}.in ${_OUTPUT_FILE} @ONLY)
    list(APPEND _gschema_OUTPUTS ${_OUTPUT_FILE})

    string(REPLACE ".xml" ".valid" file_no_xml ${file})
    set(_VALID_FILE ${CMAKE_CURRENT_BINARY_DIR}/${file_no_xml})
    list(APPEND _gschema_VALIDS ${_VALID_FILE})
    add_custom_command(
        OUTPUT ${_VALID_FILE}
        COMMAND ${CMAKE_COMMAND} -E env
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
  endforeach(file)

  set(gschema_depends ${local_depends} CACHE INTERNAL "gschemas.compiled dependencies")

  install(FILES ${_gschema_OUTPUTS} DESTINATION share/glib-2.0/schemas)

endmacro()
