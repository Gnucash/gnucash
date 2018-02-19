# Note: the set commands below can be integrated in the execute process
#       the day we require cmake > 3.1
# The command would then become
# COMMAND {CMAKE_COMMAND} -E env INTLTOOL_EXTRACT=${INTLTOOL_EXTRACT} .... ${PERL} ${INTLTOOL_UPDATE} ....

set(ENV{INTLTOOL_EXTRACT} ${INTLTOOL_EXTRACT})
set(ENV{XGETTEXT} ${XGETTEXT})
set(ENV{srcdir} ${PO_SRC_DIR})
execute_process(
  COMMAND ${PERL} ${INTLTOOL_UPDATE} -x --gettext-package ${PACKAGE} --pot
  WORKING_DIRECTORY ${PO_BIN_DIR}
  RESULT_VARIABLE GNUCASH_POT_RESULT
)
if (NOT ${GNUCASH_POT_RESULT} STREQUAL "0")
  message(FATAL_ERROR "Error when creating gnucash.pot: ${GNUCASH_POT_RESULT}")
endif()

if (NOT EXISTS ${PO_BIN_DIR}/gnucash.pot)
  message(FATAL_ERROR "POT file '${PO_BIN_DIR}/gnucash.pot' was not successfully created.")
endif()
