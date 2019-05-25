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



function(make_dist PACKAGE_PREFIX GNUCASH_SOURCE_DIR BUILD_SOURCE_DIR BUILDING_FROM_VCS)

    # -- Remove any existing packaging directory.
    file(REMOVE_RECURSE ${PACKAGE_PREFIX})

    if (EXISTS ${PACKAGE_PREFIX})
        message(FATAL_ERROR "Unable to remove existing dist directory \"${PACKAGE_PREFIX}\". Cannot continue.")
    endif()


    # -- Copy in distributed files
    if(NOT EXISTS dist_manifest.txt)
        message(FATAL_ERROR "Cannot find dist manifest: dist_manifest.txt")
    endif()

    file(STRINGS dist_manifest.txt ALL_DIST)

    foreach(file ${ALL_DIST})
        if(NOT EXISTS ${GNUCASH_SOURCE_DIR}/${file})
            message(FATAL_ERROR "Can't find dist file ${GNUCASH_SOURCE_DIR}/${file}")
        endif()
        get_filename_component(dir ${file} DIRECTORY)
        file(MAKE_DIRECTORY ${PACKAGE_PREFIX}/${dir})
        file(COPY ${GNUCASH_SOURCE_DIR}/${file} DESTINATION ${PACKAGE_PREFIX}/${dir})
    endforeach()

    # -- Copy in build products that are distributed.

    foreach(file ${dist_generated})
        execute_process(COMMAND ${CMAKE_COMMAND} -E copy ${BUILD_SOURCE_DIR}/${file} ${PACKAGE_PREFIX}/${file})
        if (NOT EXISTS ${PACKAGE_PREFIX}/${file})
            message(FATAL_ERROR "Copy of ${BUILD_SOURCE_DIR}/${file} to dist dir '${PACKAGE_PREFIX}' failed.")
        endif()
    endforeach()

    cmake_policy(SET CMP0012 NEW)

    # -- Create the tarball.

    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E tar cf ${PACKAGE_PREFIX}.tar ${PACKAGE_PREFIX}
            WORKING_DIRECTORY .
            ERROR_MSG "tar command to create ${PACKAGE_PREFIX}.tar failed."
    )

    # -- Compress the tarball with gzip
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E copy ${PACKAGE_PREFIX}.tar ${PACKAGE_PREFIX}.tar.save
    )
    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E env gzip -f ${PACKAGE_PREFIX}.tar
            WORKING_DIRECTORY .
            ERROR_MSG "gzip command to create ${PACKAGE_PREFIX}.tar.gz failed."
    )

    # -- Compress the tarball with bzip2
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E rename ${PACKAGE_PREFIX}.tar.save ${PACKAGE_PREFIX}.tar
    )
    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E env bzip2 -f ${PACKAGE_PREFIX}.tar
            WORKING_DIRECTORY .
            ERROR_MSG "bzip2 command to create ${PACKAGE_PREFIX}.tar.bz2 failed."
    )

    # -- Clean up packaging directory.

    file(REMOVE_RECURSE ${PACKAGE_PREFIX})

    if(EXISTS ${PACKAGE_PREFIX})
        message(WARNING "Could not remove packaging directory '${PACKAGE_PREFIX}'")
    endif()

    # -- All done.

    message("\n\nDistributions ${PACKAGE_PREFIX}.tar.gz and ${PACKAGE_PREFIX}.tar.bz2 created.\n\n")
endfunction()

if (NOT WITH_GNUCASH)
    message(SEND_ERROR "Creation of dist tarballs is not supported when WITH_GNUCASH=OFF.")
endif()

 make_dist(${PACKAGE_PREFIX} ${GNUCASH_SOURCE_DIR} ${BUILD_SOURCE_DIR} ${BUILDING_FROM_VCS})
