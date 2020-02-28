# Command to configure the gnucash desktop file
# These commands are stored in a separate cmake file as they have to be
# rerun depending on build conditions, not depending on cmake conditions
# (such as did the version string change or not)
#
# The following environment variables are used and should be properly set
# by the calling code:
# - SRC_DIR (top level source code directory)
# - SRC (full path to gnucash.desktop.in)
# - DST (full path to destination for gnucash.desktop)
# - VCS_INFO_FILE (full path to gnc-vcs-info.h - can be in source tree (release builds) or build tree (git builds))
# - DATADIR_BUILD (path to application data directory, typically {CMAKE_BINARY_DIR}/share)

include (${SRC_DIR}/cmake/version-info2env.cmake)
versioninfo2env (${VCS_INFO_FILE})


if (GNUCASH_BUILD_ID AND NOT "${GNUCASH_BUILD_ID}" STREQUAL "${GNC_VCS_REV}")
    set (GNC_VCS_REV "${GNC_VCS_REV} (${GNUCASH_BUILD_ID})")
endif()
configure_file(${SRC} ${DST})
