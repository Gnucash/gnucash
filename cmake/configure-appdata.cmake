# Command to configure the gnucash appdata file
# These commands are stored in a separate cmake file as they have to be
# rerun depending on build conditions, not depending on cmake conditions
# (such as did the version string change or not)
#
# Logic is as follows:
# - the source file <current-source-dir>/releases.xml will be
#   configured with version information found in gnc-vcs-info.h
#   and an optional GNUCASH_BUILD_ID variable
# - the result of this configuration will be applied to the
#   appdata input file
# The default releases.xml file will just be a single release line in
# appdata <release> node format. Handling this in a separate file allows
# packagers to override it to supply their own (package) release details.
#
# The following environment variables are used and should be properly set
# by the calling code:
# - SRC_DIR (top level source code directory)
# - SRC (full path to gnucash.appdata.xml.in)
# - DST (full path to destination for gnucash.appdata.xml)
# - REL_FILE (path to file containg (packaging) release info)
# - VCS_INFO_FILE (full path to gnc-vcs-info.h - can be in source tree (release builds) or build tree (git builds))
# - GNUCASH_BUILD_ID (optional, extra version information supplied by packagers)

include (${SRC_DIR}/cmake/version-info2env.cmake)
versioninfo2env (${VCS_INFO_FILE})
if (GNUCASH_BUILD_ID AND NOT "${GNUCASH_BUILD_ID}" STREQUAL "${GNC_VCS_REV}")
    set (GNC_VCS_REV "${GNC_VCS_REV} (${GNUCASH_BUILD_ID})")
endif()

file (READ ${REL_FILE} REL_INFO_IN)
string(CONFIGURE "${REL_INFO_IN}" REL_INFO)
configure_file(${SRC} ${DST})
