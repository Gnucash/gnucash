# Create the gnc-vcs-info.h file starting from git.
# It currently sets four parameters
# - GNC_VCS_REV
# - GNC_VCS_REV_DATE
# - GNC_VCS_REV_YEAR
# - GNC_VCS_REV_Y_M
# The following environment variables are used and should be properly set
# by the calling code:
# - SHELL (should point at a bash shell or compatible)
# - SRC_DIR (top level source code directory)
# - SRC (full path to gnc-vcs-info.h.in)
# - DST (full path to destination for gnc-vcs-info.h)


execute_process(
    COMMAND ${SHELL} "${SRC_DIR}/util/gnc-vcs-info" -r "${SRC_DIR}"
    OUTPUT_VARIABLE GNC_VCS_REV
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND ${SHELL} "${SRC_DIR}/util/gnc-vcs-info" -d "${SRC_DIR}"
    OUTPUT_VARIABLE GNC_VCS_REV_DATE
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(SUBSTRING ${GNC_VCS_REV_DATE} 0 4 GNC_VCS_REV_YEAR)
string(SUBSTRING ${GNC_VCS_REV_DATE} 0 7 GNC_VCS_REV_Y_M)
configure_file(${SRC} ${DST} @ONLY)
