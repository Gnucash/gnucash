# This file implements the process of making source distributio tarballs. It expects to find list in
# 'dist_manifest.txt' of all of the files to be included in the distribution, EXCEPT those
# files that are generated. The list of generated files is specified in MakeDistFiles.cmake in the
# COPY_FROM_BUILD and COPY_FROM_BUILD_2 variables.
#
# Given all of these files, the procedure is to:
# 1. Remove any existing dist directory and make a new one.
# 2. Copy of all the files in dist_manifest.text, COPY_FROM_BUILD and COPY_FROM_BUILD_2
#    into the dist directory.
# 3. Run autogen.sh if build a dist from Git.
# 4. Create the tarball and compress it with gzip and bzip2.
# 5. Then remove the dist directory.

include(${CMAKE_MODULE_PATH}/MakeDistFiles.cmake)

FUNCTION(FIND_AUTOMAKE AUTOMAKE_VAR ACLOCAL_VAR AUTOMAKE_VERSION_VAR NEED_OVERRIDE_VAR)
    FIND_PROGRAM(AUTOMAKE automake)
    EXECUTE_PROCESS(
            COMMAND ${AUTOMAKE} --version
            RESULT_VARIABLE AUTOMAKE_RESULT
            OUTPUT_VARIABLE AUTOMAKE_OUTPUT
            ERROR_VARIABLE AUTOMAKE_ERROR
    )

    SET(AUTOMAKE_OK TRUE)
    SET(NEED_OVERRIDE FALSE)
    IF(${AUTOMAKE} STREQUAL AUTOMAKE-NOTFOUND)
      SET(AUTOMAKE_OK FALSE)
    ELSE()
      STRING(REGEX REPLACE ".*automake \\(GNU automake\\) ([0-9]\\.[0-9]+).*" "\\1" AUTOMAKE_VERSION "${AUTOMAKE_OUTPUT}")
      FIND_PROGRAM(ACLOCAL aclocal)
      IF(${ACLOCAL} STREQUAL ACLOCAL-NOTFOUND)
        MESSAGE(FATAL_ERROR "Found ok version of automake, but can't find aclocal")
      ENDIF()
    ENDIF()
    IF (NOT AUTOMAKE_OK)
      MESSAGE(FATAL_ERROR "Can't find 'automake' or 'automake-1.11'")
      MESSAGE("  You can set AUTOTOOLS_IN_DIST=OFF to exclude autotools support.")
    ENDIF()
    SET(${AUTOMAKE_VAR} ${AUTOMAKE} PARENT_SCOPE)
    SET(${ACLOCAL_VAR} ${ACLOCAL} PARENT_SCOPE)
    SET(${AUTOMAKE_VERSION_VAR} ${AUTOMAKE_VERSION} PARENT_SCOPE)
    SET(${NEED_OVERRIDE_VAR} ${NEED_OVERRIDE} PARENT_SCOPE)
ENDFUNCTION()


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

    FOREACH(file ${COPY_FROM_BUILD} ${COPY_FROM_BUILD_2})
        EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy ${BUILD_SOURCE_DIR}/${file} ${PACKAGE_PREFIX}/${file})
        IF (NOT EXISTS ${PACKAGE_PREFIX}/${file})
            MESSAGE(FATAL_ERROR "Copy of ${BUILD_SOURCE_DIR}/${file} to dist dir '${PACKAGE_PREFIX}' failed.")
        ENDIF()
    ENDFOREACH()


    CMAKE_POLICY(SET CMP0012 NEW)

    IF (${BUILDING_FROM_VCS} AND AUTOTOOLS_IN_DIST)
        FIND_AUTOMAKE(AUTOMAKE ACLOCAL AUTOMAKE_VERSION NEED_OVERRIDE)
        # -- Run autogen.sh to cause Makefile.in files to be created.
        SET(CMAKE_COMMAND_AUTOTOOLS ${SHELL} -c)
        IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
            SET(CMAKE_COMMAND_AUTOTOOLS ${CMAKE_COMMAND} -E env)
        ENDIF()
        IF (${NEED_OVERRIDE})
          EXECUTE_PROCESS(
                  COMMAND ${CMAKE_COMMAND_AUTOTOOLS} AUTOMAKE=${AUTOMAKE} ACLOCAL=${ACLOCAL} ./autogen.sh
                  WORKING_DIRECTORY ${PACKAGE_PREFIX}
                  RESULT_VARIABLE AUTOGEN_RESULT
                  OUTPUT_VARIABLE AUTOGEN_OUTPUT
          )
        ELSE()
            EXECUTE_PROCESS(
                    COMMAND ${CMAKE_COMMAND_AUTOTOOLS} ./autogen.sh
                    WORKING_DIRECTORY ${PACKAGE_PREFIX}
                    RESULT_VARIABLE AUTOGEN_RESULT
                    OUTPUT_VARIABLE AUTOGEN_OUTPUT
            )
        ENDIF()
        IF(NOT ${AUTOGEN_RESULT} STREQUAL "0")
            MESSAGE(FATAL_ERROR "autogen.sh step failed: ${AUTOGEN_RESULT}")
        ENDIF()
        # -- Remove autogen files as they are not distributed.

        FILE(REMOVE ${PACKAGE_PREFIX}/autogen.sh)
        FILE(REMOVE_RECURSE ${PACKAGE_PREFIX}/autom4te.cache)

        # -- Autogen.sh creates some files a symbolic links that we turn into real files here.

        IF (UNIX) # No symbolic links on Windows
            SET(LINKS missing config.guess COPYING depcomp INSTALL install-sh config.sub compile)
            IF(${AUTOMAKE_VERSION} VERSION_GREATER 1.11)
                LIST(APPEND LINKS test-driver)
            ENDIF()
            FOREACH(link ${LINKS})
                GET_FILENAME_COMPONENT(realpath ${PACKAGE_PREFIX}/${link} REALPATH)
                FILE(REMOVE ${PACKAGE_PREFIX}/${link})
                FILE(COPY ${realpath} DESTINATION ${PACKAGE_PREFIX})
            ENDFOREACH(link)

            FOREACH(link src/doc/design/mdate-sh src/doc/design/texinfo.tex)
                GET_FILENAME_COMPONENT(dir ${link} DIRECTORY)
                GET_FILENAME_COMPONENT(realpath ${PACKAGE_PREFIX}/${link} REALPATH)
                FILE(REMOVE ${PACKAGE_PREFIX}/${link})
                FILE(COPY ${realpath} DESTINATION ${PACKAGE_PREFIX}/${dir})
            ENDFOREACH(link)
        ENDIF(UNIX)

    ENDIF()

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

MAKE_DIST(${PACKAGE_PREFIX} ${GNUCASH_SOURCE_DIR} ${BUILD_SOURCE_DIR} ${BUILDING_FROM_VCS})
