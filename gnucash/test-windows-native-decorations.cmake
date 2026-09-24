# Verify the Windows runtime environment selects native window decorations.

if (NOT DEFINED ENVIRONMENT_FILE OR NOT EXISTS "${ENVIRONMENT_FILE}")
  message(FATAL_ERROR "GnuCash environment file not found: ${ENVIRONMENT_FILE}")
endif()

file(STRINGS "${ENVIRONMENT_FILE}" gtk_csd_lines REGEX "^GTK_CSD=")
list(LENGTH gtk_csd_lines gtk_csd_count)

if (NOT gtk_csd_count EQUAL 1 OR NOT gtk_csd_lines STREQUAL "GTK_CSD=0")
  message(FATAL_ERROR
    "Expected exactly one GTK_CSD=0 entry in ${ENVIRONMENT_FILE}; "
    "found: ${gtk_csd_lines}")
endif()
