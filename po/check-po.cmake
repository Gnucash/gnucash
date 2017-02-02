
# Run intltool-update -m to check for missing files in POTFILES.in
# We detect failure by looking for the presence of a 'missing' file.

# Intltool returns a zero status whether or not the check failed.

FILE(REMOVE ${PO_DIR}/missing)
IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
  EXECUTE_PROCESS(
          COMMAND ${CMAKE_COMMAND} -E env ${INTLTOOL_UPDATE} -m
          WORKING_DIRECTORY ${PO_DIR}
  )
ELSE()
  EXECUTE_PROCESS(
          COMMAND ${INTLTOOL_UPDATE} -m
          WORKING_DIRECTORY ${PO_DIR}
          RESULT_VARIABLE UPDATE_RESULT
  )
  MESSAGE("UPDATE_RESULT = ${UPDATE_RESULT}")
ENDIF()
IF (EXISTS ${PO_DIR}/missing)
  MESSAGE(FATAL_ERROR "POTFILES.in is missing files. See 'missing' in ${PO_DIR}")
ENDIF()
