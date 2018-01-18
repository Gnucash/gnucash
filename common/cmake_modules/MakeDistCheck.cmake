
include(MakeDistFiles)

FUNCTION(RUN_DIST_CHECK PACKAGE_PREFIX EXT)

    SET(tarball ${PACKAGE_PREFIX}.tar${EXT})
    IF (NOT EXISTS ${tarball})
        MESSAGE(FATAL_ERROR "Can't find dist tarball '${tarball}'")
    ENDIF()

    # Remove the directory we're about to extract to
    FILE(REMOVE_RECURSE ${PACKAGE_PREFIX})

    # Untar the distribution we want to check
    SET(TAR_OPTION "zxf")
    IF (${EXT} STREQUAL ".bz2")
        SET(TAR_OPTION "jxf")
    ENDIF()
        
    SET(MY_CMAKE_COMMAND "")
    IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
        SET(MY_CMAKE_COMMAND ${CMAKE_COMMAND} -E env)
    ENDIF()

    FIND_PROGRAM(NINJA_COMMAND NAMES ninja ninja-build)
    IF (${NINJA_COMMAND} STREQUAL "NINJA_COMMAND-NOTFOUND")
        MESSAGE(FATAL_ERROR "Can't find the 'ninja' or 'ninja-build' program.")
    ENDIF ()

    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${CMAKE_COMMAND} -E tar ${TAR_OPTION} ${tarball}
            WORKING_DIRECTORY .
            ERROR_MSG "Command to untar ${tarball} failed."
    )

    # Officially, I should make the contents of the untarred dist directory read-only,
    # but that will cause the build to fail (intltool is unhappy).

    # Create a build directory and configure the Cmake build

    SET(BUILD_DIR "_cmake_build")
    SET(INSTALL_DIR "_cmake_install")
    FILE(REMOVE_RECURSE ${BUILD_DIR} ${INSTALL_DIR})

    FILE(MAKE_DIRECTORY ${BUILD_DIR} ${INSTALL_DIR})

    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${CMAKE_COMMAND} -G Ninja
              -D CMAKE_C_FLAGS=${CMAKE_C_FLAGS}
              -D CMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}
              -D CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}
              -D CMAKE_INSTALL_PREFIX=../${INSTALL_DIR}
              -D AUTOTOOLS_IN_DIST=${AUTOTOOLS_IN_DIST}
              ../${PACKAGE_PREFIX}
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "CMake configure command failed."
    )

    # Run ninja in the build directory
    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${MY_CMAKE_COMMAND} ${NINJA_COMMAND}
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja build failed."
    )

    # Run ninja install

    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${MY_CMAKE_COMMAND} ${NINJA_COMMAND} install
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja install failed."
    )

    # Run ninja check in the build directory
    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${MY_CMAKE_COMMAND} ${NINJA_COMMAND} check
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja check failed."
    )

    # Run ninja dist
    EXECUTE_PROCESS_AND_CHECK_RESULT(
            COMMAND ${MY_CMAKE_COMMAND} ${NINJA_COMMAND} dist
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja dist failed."
    )

    MESSAGE("distcheck complete.")

ENDFUNCTION()

RUN_DIST_CHECK(${PACKAGE_PREFIX} .gz)
