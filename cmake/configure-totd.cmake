# Command to configure the gnucash man page
# These commands are store in a separate cmake file as they have to be
# rerun depending on build conditions, not depending on cmake conditions
# (such as did the version string change or not)
#
# The following environment variables are used and should be properly set
# by the calling code:
# - SRC_DIR (source code directory containing tip_of_the_day.list.c)
# - DST_DIR (build dir to write tip_of_the_day.list to)
# - SRC (full path to tip_of_the_day.list.c)
# - DST (target filename)
# - CMAKE_C_COMPILER (path to C compiler, used to parse the input file)
file(REMOVE ${DST_DIR}/${TOTD})
execute_process(
    COMMAND ${CMAKE_C_COMPILER} -E -P -x c -DN_\(x\)=x -o ${TOTD}.tmp ${SRC}
)

file(STRINGS ${TOTD}.tmp TIP_OF_THE_DAY_LINES ENCODING UTF-8)
set(TOTD_OUTPUT "")
foreach(line ${TIP_OF_THE_DAY_LINES})
  string(REGEX REPLACE "^ *\"" "" line2 "${line}")
  string(REGEX REPLACE "\" *$" "" line3 "${line2}")
  file(APPEND ${DST_DIR}/${TOTD} "${line3}\n")
endforeach()
