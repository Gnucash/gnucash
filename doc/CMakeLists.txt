add_subdirectory(examples)

set(doc_DATA
  gtk-3.0.css
)

set(doc_noinst_DATA
        CMakeLists.txt gnucash.1.in
        gnucash-cli.1.in tip_of_the_day.list.c README.txt)

install(FILES ${doc_DATA} DESTINATION  ${CMAKE_INSTALL_DOCDIR})

set_local_dist(doc_DIST_local ${doc_DATA} ${doc_noinst_DATA})
set(doc_DIST ${doc_DIST_local} ${examples_DIST} PARENT_SCOPE)

foreach(doc_file ${doc_DATA})
    configure_file(${doc_file} ${DATADIR_BUILD}/doc/gnucash/${doc_file} COPYONLY)
endforeach()

# Generate the tip of the day file.

set (totd "tip_of_the_day.list")
add_custom_command(OUTPUT ${DATADIR_BUILD}/gnucash/${totd}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${totd}.c
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND}
        -D SRC=${CMAKE_CURRENT_SOURCE_DIR}/${totd}.c
        -D TOTD=${totd}
        -D DST_DIR=${DATADIR_BUILD}/gnucash
        -D CMAKE_C_COMPILER=${CMAKE_C_COMPILER}
        -P ${CMAKE_SOURCE_DIR}/cmake/configure-totd.cmake
)
add_custom_target(totd ALL DEPENDS ${DATADIR_BUILD}/gnucash/${totd})

install(FILES ${DATADIR_BUILD}/gnucash/tip_of_the_day.list DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash)

# Generate manpages.

foreach (manpage gnucash gnucash-cli)
    add_custom_command(OUTPUT ${DATADIR_BUILD}/gnucash/${manpage}.1
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${manpage}.1.in gnc-vcs-info
        COMMAND ${CMAKE_COMMAND}
            -D SRC=${CMAKE_CURRENT_SOURCE_DIR}/${manpage}.1.in
            -D DST=${manpage}.1
            -D VCS_INFO_FILE=${VCS_INFO_FILE}
            -D DATADIR_BUILD=${DATADIR_BUILD}
            -D SRC_DIR=${CMAKE_SOURCE_DIR}
            -P ${CMAKE_SOURCE_DIR}/cmake/configure-manpage.cmake
    )
    add_custom_target(${manpage}-manpage ALL DEPENDS ${DATADIR_BUILD}/gnucash/${manpage}.1)

    install(FILES ${DATADIR_BUILD}/gnucash/${manpage}.1 DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)
endforeach()
