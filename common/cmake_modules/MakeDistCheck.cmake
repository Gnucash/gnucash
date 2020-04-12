
include(MakeDistFiles)

function(run_dist_check PACKAGE_PREFIX EXT)

    set(tarball ${PACKAGE_PREFIX}.tar${EXT})
    if (NOT EXISTS ${tarball})
        message(FATAL_ERROR "Can't find dist tarball '${tarball}'")
    endif()

    # Remove the directory we're about to extract to
    file(REMOVE_RECURSE ${PACKAGE_PREFIX})

    # Untar the distribution we want to check
    set(TAR_OPTION "zxf")
    if (${EXT} STREQUAL ".bz2")
        set(TAR_OPTION "jxf")
    endif()

    FIND_PROGRAM(NINJA_COMMAND NAMES ninja ninja-build)
    if (${NINJA_COMMAND} STREQUAL "NINJA_COMMAND-NOTFOUND")
        message(FATAL_ERROR "Can't find the 'ninja' or 'ninja-build' program.")
    endif()

    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E tar ${TAR_OPTION} ${tarball}
            WORKING_DIRECTORY .
            ERROR_MSG "Command to untar ${tarball} failed."
    )

    # Officially, I should make the contents of the untarred dist directory read-only,
    # but that will cause the build to fail (intltool is unhappy).

    # Create a build directory and configure the Cmake build

    set(BUILD_DIR "_cmake_build")
    set(INSTALL_DIR "_cmake_install")
    file(REMOVE_RECURSE ${BUILD_DIR} ${INSTALL_DIR})

    file(MAKE_DIRECTORY ${BUILD_DIR} ${INSTALL_DIR})

    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -G Ninja
              -D CMAKE_C_FLAGS=${CMAKE_C_FLAGS}
              -D CMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}
              -D CMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}
              -D CMAKE_INSTALL_PREFIX=../${INSTALL_DIR}
              -D GTEST_ROOT=${GTEST_ROOT}
              -D GMOCK_ROOT=${GMOCK_ROOT}
              -D WITH_PYTHON=YES
              ../${PACKAGE_PREFIX}
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "CMake configure command failed."
    )

    # Run ninja in the build directory
    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E env ${NINJA_COMMAND}
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja build failed."
    )

    # Run ninja install
    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E env ${NINJA_COMMAND} install
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja install failed."
    )

    # Run ninja check in the build directory
    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E env ${NINJA_COMMAND} check
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja check failed."
    )

    # Run ninja dist
    execute_process_and_check_result(
            COMMAND ${CMAKE_COMMAND} -E env ${NINJA_COMMAND} dist
            WORKING_DIRECTORY ${BUILD_DIR}
            ERROR_MSG "Ninja dist failed."
    )

    message("distcheck complete.")

endfunction()

run_dist_check(${PACKAGE_PREFIX} .gz)
