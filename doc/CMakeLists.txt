ADD_SUBDIRECTORY(examples)

SET(doc_DATA
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
)

SET(doc_noinst_DATA
        build-aix.txt build-solaris.txt CMakeLists.txt gnc-fq-dump.1 gnc-fq-helper.1 gnucash.1.in gtkrc-2.0.gnucash
        misc-notes.txt README.build-system README.HBCI README.OFX README.translator.txt tip_of_the_day.list.in
        TRANSLATION_HOWTO)

INSTALL(FILES ${doc_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/doc/gnucash)

SET_LOCAL_DIST(doc_DIST_local ${doc_DATA} ${doc_noinst_DATA})
SET(doc_DIST ${doc_DIST_local} ${examples_DIST} PARENT_SCOPE)

FILE(COPY ${doc_DATA} DESTINATION ${DATADIR_BUILD}/doc/gnucash)

# Generate the tip of the day file.

EXECUTE_PROCESS(
    COMMAND ${CMAKE_C_COMPILER} -E -P -x c -DN_\(x\)=x -o ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list.tmp ${CMAKE_CURRENT_SOURCE_DIR}/tip_of_the_day.list.in
)

FILE(STRINGS ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list.tmp TIP_OF_THE_DAY_LINES)

SET(TOTD_OUTPUT "")
FOREACH(line ${TIP_OF_THE_DAY_LINES})
  STRING(REGEX REPLACE "^ *\"" "" line2 "${line}")
  STRING(REGEX REPLACE "\" *$" "" line3 "${line2}")
  LIST(APPEND TOTD_OUTPUT "${line3}\n")
ENDFOREACH()

LIST(APPEND TOTD_OUTPUT "\n")

STRING(CONCAT FINAL_TOTD ${TOTD_OUTPUT})

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list "${FINAL_TOTD}")

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash)

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list
  DESTINATION ${DATADIR_BUILD}/gnucash)

# ---
IF (BUILDING_FROM_VCS)
  SET(VCS_INFO_BASE_DIR ${CMAKE_BINARY_DIR})
ELSE()
  SET(VCS_INFO_BASE_DIR ${CMAKE_SOURCE_DIR})
ENDIF()
SET(VCS_INFO_FILE ${VCS_INFO_BASE_DIR}/libgnucash/core-utils/gnc-vcs-info.h)

# The copious use of backslashes below is to escape escape sequences that
# have to end up in the file being written...
# eg \\\" will become \", \\\\1 will become \\1 (which cmake will then interpret as \1)
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/manpage.cmake
"FILE(STRINGS \${VCS_INFO_FILE} ym_line REGEX \"GNC_VCS_REV_Y_M\")
STRING(REGEX REPLACE \"^.* \\\"(.*)\\\"\" \"\\\\1\" DATE \${ym_line})
CONFIGURE_FILE(\${SRC} \${DST} )
FILE(COPY gnucash.1
  DESTINATION \${DATADIR_BUILD}/gnucash)
")


ADD_CUSTOM_COMMAND(OUTPUT gnucash.1
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gnucash.1.in gnc-vcs-info
  COMMAND ${CMAKE_COMMAND} -D SRC=${CMAKE_CURRENT_SOURCE_DIR}/gnucash.1.in
                           -D DST=gnucash.1
                           -D VERSION=${VERSION}
                           -D VCS_INFO_FILE=${VCS_INFO_FILE}
                           -D DATADIR_BUILD=${DATADIR_BUILD}
                           -P ${CMAKE_CURRENT_BINARY_DIR}/manpage.cmake
  )
ADD_CUSTOM_TARGET(gnucash-manpage DEPENDS gnucash.1)

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.1 DESTINATION  ${CMAKE_INSTALL_MANDIR}/man1)
