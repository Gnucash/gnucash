# This file implements the process of making source distribution tarballs. It expects to find list in
# 'dist_manifest.txt' of all of the files to be included in the distribution, EXCEPT those
# files that are generated. The list of generated files handled via the 'dist_generated' cmake cache variable
#
# Given all of these files, the procedure is to:
# 1. Remove any existing dist directory and make a new one.
# 2. Copy of all the files in dist_manifest.text and ${dist_generated}
#    into the dist directory.
# 3. Create the tarball and compress it with gzip and bzip2.
# 4. Then remove the dist directory.

include(${CMAKE_MODULE_PATH}/MakeDistFiles.cmake)



FUNCTION(MAKE_DIST PACKAGE_PREFIX GNUCASH_SOURCE_DIR BUILD_SOURCE_DIR BUILDING_FROM_VCS)

    SET(CMAKE_COMMAND_TMP "")
    IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
        SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
    ENDIF()

    # -- Remove any existing packaging directory.
    FILE(REMOVE_RECURSE ${PACKAGE_PREFIX})

    IF (EXISTS ${PACKAGE_PREFIX})
        MESSAGE(FATAL_ERROR "Unable to remove existing dist directory \"${PACKAGE_PREFIX}\". Cannot continue.")
    ENDIF()


    # -- Copy in distributed files
    IF(NOT EXISTS dist_manifest.txt)
        message(FATAL_ERROR "Cannot find dist manifest: dist_manifest.txt")
    ENDIF()

    file(STRINGS dist_manifest.txt ALL_DIST)

    FOREACH(file ${ALL_DIST})
        IF(NOT EXISTS ${GNUCASH_SOURCE_DIR}/${file})
            MESSAGE(FATAL_ERROR "Can't find dist file ${GNUCASH_SOURCE_DIR}/${file}")
        ENDIF()
        GET_FILENAME_COMPONENT(dir ${file} DIRECTORY)
        FILE(MAKE_DIRECTORY ${PACKAGE_PREFIX}/${dir})
        FILE(COPY ${GNUCASH_SOURCE_DIR}/${file} DESTINATION ${PACKAGE_PREFIX}/${dir})
    ENDFOREACH()

    # -- Copy in build products that are distributed.

    FOREACH(file ${dist_generated})
        EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy ${BUILD_SOURCE_DIR}/${file} ${PACKAGE_PREFIX}/${file})
        IF (NOT EXISTS ${PACKAGE_PREFIX}/${file})
            MESSAGE(FATAL_ERROR "Copy of ${BUILD_SOURCE_DIR}/${file} to dist dir '${PACKAGE_PREFIX}' failed.")
        ENDIF()
    ENDFOREACH()

    CMAKE_POLICY(SET CMP0012 NEW)

    # -- Create the tarball.

    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${CMAKE_COMMAND} -E tar cf ${PACKAGE_PREFIX}.tar ${PACKAGE_PREFIX}
            WORKING_DIRECTORY .
            ERROR_MSG "tar command to create ${PACKAGE_PREFIX}.tar failed."
    )

    # -- Compress the tarball with gzip
    EXECUTE_PROCESS(
        COMMAND ${CMAKE_COMMAND} -E copy ${PACKAGE_PREFIX}.tar ${PACKAGE_PREFIX}.tar.save
    )
    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${CMAKE_COMMAND_TMP} gzip -f ${PACKAGE_PREFIX}.tar
            WORKING_DIRECTORY .
            ERROR_MSG "gzip command to create ${PACKAGE_PREFIX}.tar.gz failed."
    )

    # -- Compress the tarball with bzip2
    EXECUTE_PROCESS(
        COMMAND ${CMAKE_COMMAND} -E rename ${PACKAGE_PREFIX}.tar.save ${PACKAGE_PREFIX}.tar
    )
    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${CMAKE_COMMAND_TMP} bzip2 -f ${PACKAGE_PREFIX}.tar
            WORKING_DIRECTORY .
            ERROR_MSG "bzip2 command to create ${PACKAGE_PREFIX}.tar.bz2 failed."
    )

    # -- Clean up packaging directory.

    FILE(REMOVE_RECURSE ${PACKAGE_PREFIX})

    IF(EXISTS ${PACKAGE_PREFIX})
        MESSAGE(WARNING "Could not remove packaging directory '${PACKAGE_PREFIX}'")
    ENDIF()

    # -- All done.

    MESSAGE("\n\nDistributions ${PACKAGE_PREFIX}.tar.gz and ${PACKAGE_PREFIX}.tar.bz2 created.\n\n")
ENDFUNCTION()

IF (NOT WITH_GNUCASH)
    MESSAGE(SEND_ERROR "Creation of dist tarballs not support when WITH_GNUCASH=OFF.")
ENDIF()

 MAKE_DIST(${PACKAGE_PREFIX} ${GNUCASH_SOURCE_DIR} ${BUILD_SOURCE_DIR} ${BUILDING_FROM_VCS})
