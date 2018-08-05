add_subdirectory(examples)

set(doc_DATA
  README.francais
  README.german
  README-ca.win32-bin.txt
  README-de.win32-bin.txt
  README-fr.win32-bin.txt
  README-it.win32-bin.txt
  README-lv.win32-bin.txt
  README-nl.win32-bin.txt
  README-zh_CN.win32-bin.txt
  README-zh_TW.win32-bin.txt
  README.win32-bin.txt
  guile-hackers.txt
  projects.html
  gtk-3.0.css
)

set(doc_noinst_DATA
        build-aix.txt build-solaris.txt CMakeLists.txt gnc-fq-dump.1 gnc-fq-helper.1 gnucash.1.in
        misc-notes.txt README.HBCI README.OFX README.translator.txt tip_of_the_day.list.c
        TRANSLATION_HOWTO)

install(FILES ${doc_DATA} DESTINATION  ${CMAKE_INSTALL_DOCDIR})

set_local_dist(doc_DIST_local ${doc_DATA} ${doc_noinst_DATA})
set(doc_DIST ${doc_DIST_local} ${examples_DIST} PARENT_SCOPE)

file(COPY ${doc_DATA} DESTINATION ${DATADIR_BUILD}/doc/gnucash)

# Generate the tip of the day file.

execute_process(
    COMMAND ${CMAKE_C_COMPILER} -E -P -x c -DN_\(x\)=x -o ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list.tmp ${CMAKE_CURRENT_SOURCE_DIR}/tip_of_the_day.list.c
)

file(STRINGS ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list.tmp TIP_OF_THE_DAY_LINES)

set(TOTD_OUTPUT "")
foreach(line ${TIP_OF_THE_DAY_LINES})
  string(REGEX REPLACE "^ *\"" "" line2 "${line}")
  string(REGEX REPLACE "\" *$" "" line3 "${line2}")
  list(APPEND TOTD_OUTPUT "${line3}\n")
endforeach()

string(CONCAT FINAL_TOTD ${TOTD_OUTPUT})

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list "${FINAL_TOTD}")

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash)

file(COPY ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list
  DESTINATION ${DATADIR_BUILD}/gnucash)

# ---
if (BUILDING_FROM_VCS)
  set(VCS_INFO_BASE_DIR ${CMAKE_BINARY_DIR})
else()
  set(VCS_INFO_BASE_DIR ${CMAKE_SOURCE_DIR})
endif()
set(VCS_INFO_FILE ${VCS_INFO_BASE_DIR}/libgnucash/core-utils/gnc-vcs-info.h)

# The copious use of backslashes below is to escape escape sequences that
# have to end up in the file being written...
# eg \\\" will become \", \\\\1 will become \\1 (which cmake will then interpret as \1)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/manpage.cmake
"file(STRINGS \${VCS_INFO_FILE} ym_line REGEX \"GNC_VCS_REV_Y_M\")
string(REGEX REPLACE \"^.* \\\"(.*)\\\"\" \"\\\\1\" DATE \${ym_line})
configure_file(\${SRC} \${DST} )
file(COPY gnucash.1
  DESTINATION \${DATADIR_BUILD}/gnucash)
")


add_custom_command(OUTPUT gnucash.1
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gnucash.1.in gnc-vcs-info
  COMMAND ${CMAKE_COMMAND} -D SRC=${CMAKE_CURRENT_SOURCE_DIR}/gnucash.1.in
                           -D DST=gnucash.1
                           -D VERSION=${VERSION}
                           -D VCS_INFO_FILE=${VCS_INFO_FILE}
                           -D DATADIR_BUILD=${DATADIR_BUILD}
                           -P ${CMAKE_CURRENT_BINARY_DIR}/manpage.cmake
  )
add_custom_target(gnucash-manpage DEPENDS gnucash.1)
dist_add_generated (${BUILDING_FROM_VCS} gnucash.1)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.1 DESTINATION  ${CMAKE_INSTALL_MANDIR}/man1)
