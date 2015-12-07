MACRO(ADD_GSCHEMA_TARGETS _TARGET _gschema_INPUTS)
  SET(_gschema_OUTPUTS "")
  SET(_gschema_VALIDS "")
  SET(_gschema_BUILDS "")
  # FIXME: I have no idea of I'm using the right options here for intltool-merge for Windows.
  SET(INITTOOL_OPTIONS "--no-translations")
  IF(WIN32)
    SET(INITTOOL_OPTIONS "/tmp")
  ENDIF(WIN32)
  FOREACH(file ${_gschema_INPUTS})
    GNC_CONFIGURE2(${file}.in.in ${file}.in)
    STRING(REPLACE ".xml" ".valid" file_no_xml ${file})
    SET(_OUTPUT_FILE ${CMAKE_CURRENT_BINARY_DIR}/${file})
    SET(_BUILD_FILE ${DATADIR_BUILD}/glib-2.0/schemas/${file})
    SET(_VALID_FILE ${CMAKE_CURRENT_BINARY_DIR}/${file_no_xml})
    LIST(APPEND _gschema_OUTPUTS ${_OUTPUT_FILE})
    LIST(APPEND _gschema_VALIDS ${_VALID_FILE})
    IF (GNC_BUILD_AS_INSTALL)
      LIST(APPEND _gschema_BUILDS ${_BUILD_FILE})
    ENDIF()
    ADD_CUSTOM_COMMAND(
        OUTPUT ${_OUTPUT_FILE}
        COMMAND ${CMAKE_COMMAND} -E env
          LC_ALL=C
          ${PERL_EXECUTABLE} ${INTLTOOL_MERGE} -x -u ${INITTOOL_OPTIONS} ${CMAKE_CURRENT_BINARY_DIR}/${file}.in ${CMAKE_CURRENT_BINARY_DIR}/${file}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.in
        MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${file}.in.in
    )
    IF (GNC_BUILD_AS_INSTALL)
      ADD_CUSTOM_COMMAND(
        OUTPUT ${_BUILD_FILE}
        COMMAND ${CMAKE_COMMAND} -E copy ${_OUTPUT_FILE} ${_BUILD_FILE}
        DEPENDS ${_OUTPUT_FILE}
      )
    ENDIF()
    ADD_CUSTOM_COMMAND(
        OUTPUT ${_VALID_FILE}
        COMMAND ${CMAKE_COMMAND} -E env
        ${GLIB_COMPILE_SCHEMAS} --strict --dry-run --schema-file=${_OUTPUT_FILE}
        COMMAND ${CMAKE_COMMAND} -E touch ${_VALID_FILE}
        DEPENDS ${_OUTPUT_FILE}
    )
  ENDFOREACH(file)

  ADD_CUSTOM_TARGET(${_TARGET} DEPENDS "${_gschema_OUTPUTS};${_gschema_VALIDS};${_gschema_BUILDS}")

  INSTALL(FILES ${_gschema_OUTPUTS} DESTINATION share/glib-2.0/schemas)

ENDMACRO()
