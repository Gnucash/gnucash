execute_process(
    COMMAND ${XGETTEXT} --add-comments
                        --directory=${TOP_SRC_DIR}
                        --default-domain=${PACKAGE}
                        --output=${PACKAGE}.pot
                        --files-from=${PO_SRC_DIR}/POTFILES.in
                        --from-code=UTF-8
                        --flag=g_strdup_printf:1:c-format
                        --flag=g_string_printf:2:c-format
                        --flag=g_string_append_printf:2:c-format
                        --flag=g_error_new:3:c-format
                        --flag=g_set_error:4:c-format
                        --flag=g_markup_printf_escaped:1:c-format
                        --flag=g_log:3:c-format
                        --flag=g_print:1:c-format
                        --flag=g_printerr:1:c-format
                        --flag=g_printf:1:c-format
                        --flag=g_fprintf:2:c-format
                        --flag=g_sprintf:2:c-format
                        --flag=g_snprintf:3:c-format
                        --flag=g_scanner_error:2:c-format
                        --flag=g_scanner_warn:2:c-format
                        --keyword=_
                        --keyword=N_
                        --keyword=Q_
                        --keyword=translate:1,1t
                        --keyword=translate:1c,2,2t
                        --keyword=translate:1,2,3t
                        --keyword=translate:1c,2,3,4t
                        --package-name=${PACKAGE_NAME}
                        --package-version=${PACKAGE_VERSION}
                        --msgid-bugs-address=https://bugzilla.gnome.org/enter_bug.cgi?product=GnuCash&component=Translations
  WORKING_DIRECTORY ${PO_BIN_DIR}
  RESULT_VARIABLE GNUCASH_POT_RESULT
)
if (NOT ${GNUCASH_POT_RESULT} STREQUAL "0")
  message(FATAL_ERROR "Error when creating gnucash.pot: ${GNUCASH_POT_RESULT}")
endif()

if (NOT EXISTS ${PO_BIN_DIR}/gnucash.pot)
  message(FATAL_ERROR "POT file '${PO_BIN_DIR}/gnucash.pot' was not successfully created.")
endif()
