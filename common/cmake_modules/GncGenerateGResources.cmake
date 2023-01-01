# gnc_generate_gresources (BASE filename
#                          RESOURCE_FILES resource1 resource2 ...)
#
# Function to generate two files in CMAKE_CURRENT_BINARY_DIR:
# - a gresource xml file that serves as an input to the glib_resource_compiler
# - a c source file to be compiled and linked with a library or executable
#   to include the resources listed in RESOURCE_FILES in that library or
#   executable
#
# To link the resources, add
#
#   ${CMAKE_CURRENT_BINARY_DIR}/<filename>.c
#
# as additional source to the relevant "add_library" or "add_executable" call.
#
#
# BASE filename
#    the base filename without extension for all output files to generate
#
# RESOURCE_FILES
#    a list of files that you want compiled into a gresource
#
# The XML file will be generated in the current cmake binary directory
function(gnc_generate_gresources)

    set(singleValues BASE)
    set(multiValues RESOURCE_FILES)
    cmake_parse_arguments(GR "" "${singleValues}" "${multiValues}" ${ARGN})

    string(STRIP GR_BASE "${GR_BASE}")
    set(XML_FILE "${GR_BASE}.xml")

    set(TMP_FILE "${CMAKE_CURRENT_BINARY_DIR}/${XML_FILE}-tmp")
    set(XML_PATH "${CMAKE_CURRENT_BINARY_DIR}/${XML_FILE}")

    file(WRITE ${TMP_FILE} "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    file(APPEND ${TMP_FILE} "<gresources>\n")
    file(APPEND ${TMP_FILE} "  <gresource prefix=\"${GNUCASH_RESOURCE_PREFIX}\">\n")

    foreach(res_file ${GR_RESOURCE_FILES})
        get_filename_component(res_file_short ${res_file} NAME)
        file(APPEND ${TMP_FILE} "    <file alias=\"${res_file_short}\">${res_file}</file>\n")
    endforeach()
    file(APPEND ${TMP_FILE} "  </gresource>\n")
    file(APPEND ${TMP_FILE} "</gresources>\n")

    # Regenerate target file only if something changed
    configure_file(${TMP_FILE} ${XML_PATH} COPYONLY)
    file(REMOVE ${TMP_FILE})

    set(C_FILE "${GR_BASE}.c")
    add_custom_command(
        OUTPUT ${C_FILE}
        COMMAND
            "${GLIB_COMPILE_RESOURCES_EXECUTABLE}"
                --target=${C_FILE}
                --sourcedir=${CMAKE_CURRENT_SOURCE_DIR}
                --generate-source
                ${XML_PATH}
        DEPENDS
            ${XML_PATH} ${GR_RESOURCE_FILES}
    )
endfunction()
