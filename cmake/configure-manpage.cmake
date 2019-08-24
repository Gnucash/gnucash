# Command to configure the gnucash man page
# These commands are store in a separate cmake file as they have to be
# rerun depending on build conditions, not depending on cmake conditions
# (such as did the version string change or not)
#
# The following environment variables are used and should be properly set
# by the calling code:
# - SRC_DIR (top level source code directory)
# - SRC (full path to gnc-vcs-info.h.in)
# - DST (full path to destination for gnc-vcs-info.h)
# - VCS_INFO_FILE (full path to gnc-vcs-info.h - can be in source tree (release builds) or build tree (git builds))

include (${SRC_DIR}/cmake/version-info2env.cmake)
versioninfo2env (${VCS_INFO_FILE})
configure_file(${SRC} ${DST} )
file(COPY gnucash.1
DESTINATION ${DATADIR_BUILD}/gnucash)
