# Create the gnc-vcs-info.h file starting from a source directory that's
# - not a git working directory
# - not extracted from a distribution tarball
# It currently sets four parameters
# - GNC_VCS_REV
# - GNC_VCS_REV_DATE
# - GNC_VCS_REV_YEAR
# - GNC_VCS_REV_Y_M
# The following environment variables are used and should be properly set
# by the calling code:
# - PROJECT_VERSION
# - SRC (full path to gnc-vcs-info.h.in)
# - DST (full path to destination for gnc-vcs-info.h)

set (GNC_VCS_REV "${PROJECT_VERSION}-unknown-commit")
string (TIMESTAMP GNC_VCS_REV_DATE "%Y-%m-%d")
string(SUBSTRING ${GNC_VCS_REV_DATE} 0 4 GNC_VCS_REV_YEAR)
string(SUBSTRING ${GNC_VCS_REV_DATE} 0 7 GNC_VCS_REV_Y_M)
configure_file(${SRC} ${DST} @ONLY)
