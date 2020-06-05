add_subdirectory(examples)

set(doc_DATA
  gtk-3.0.css
)

set(doc_noinst_DATA
        CMakeLists.txt gnc-fq-dump.1 gnc-fq-helper.1 gnucash.1.in
        gnucash-cli.1.in tip_of_the_day.list.c README.txt)

install(FILES ${doc_DATA} DESTINATION  ${CMAKE_INSTALL_DOCDIR})

set_local_dist(doc_DIST_local ${doc_DATA} ${doc_noinst_DATA})
set(doc_DIST ${doc_DIST_local} ${examples_DIST} PARENT_SCOPE)

foreach(doc_file ${doc_DATA})
    configure_file(${doc_file} ${DATADIR_BUILD}/doc/gnucash/${doc_file} COPYONLY)
endforeach()

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

foreach (manpage gnucash gnucash-cli)
    add_custom_command(OUTPUT ${manpage}.1
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${manpage}.1.in ${VCS_INFO_FILE}
    COMMAND ${CMAKE_COMMAND} -D SRC=${CMAKE_CURRENT_SOURCE_DIR}/${manpage}.1.in
                            -D DST=${manpage}.1
                            -D VCS_INFO_FILE=${VCS_INFO_FILE}
                            -D DATADIR_BUILD=${DATADIR_BUILD}
                            -D SRC_DIR=${CMAKE_SOURCE_DIR}
                            -P ${CMAKE_SOURCE_DIR}/cmake/configure-manpage.cmake
    )
    add_custom_target(${manpage}-manpage DEPENDS ${manpage}.1)
    dist_add_generated (${BUILDING_FROM_VCS} ${manpage}.1)

    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.1 DESTINATION  ${CMAKE_INSTALL_MANDIR}/man1)
endforeach()
