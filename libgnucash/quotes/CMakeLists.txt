
set(_BIN_FILES "")
foreach(file gnc-fq-check.in gnc-fq-helper.in gnc-fq-update.in gnc-fq-dump.in)
  string(REPLACE ".in" "" _OUTPUT_FILE_NAME ${file})
  set(_ABS_OUTPUT_FILE ${BINDIR_BUILD}/${_OUTPUT_FILE_NAME})
  configure_file( ${file} ${_ABS_OUTPUT_FILE} @ONLY)
  list(APPEND _BIN_FILES ${_ABS_OUTPUT_FILE})
endforeach(file)


set(_MAN_FILES "")
foreach(file gnc-fq-dump gnc-fq-helper)
  set(_POD_INPUT ${BINDIR_BUILD}/${file})
  set(_MAN_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}.1)
  list(APPEND _MAN_FILES ${_MAN_OUTPUT})
  add_custom_command(
      OUTPUT ${_MAN_OUTPUT}
      COMMAND ${CMAKE_COMMAND} -E env ${PERL_EXECUTABLE} ${POD2MAN_EXECUTABLE} ${_POD_INPUT} ${_MAN_OUTPUT}
      DEPENDS ${_POD_INPUT}
  )
endforeach(file)


add_custom_target(quotes-man ALL DEPENDS ${_MAN_FILES})
add_custom_target(quotes-bin ALL DEPENDS ${_BIN_FILES})
install(FILES ${_MAN_FILES} DESTINATION  ${CMAKE_INSTALL_MANDIR}/man1)
install(PROGRAMS ${_BIN_FILES} DESTINATION ${CMAKE_INSTALL_BINDIR})

set_dist_list(quotes_DIST CMakeLists.txt gnc-fq-check.in gnc-fq-dump.in gnc-fq-helper.in gnc-fq-update.in Quote_example.pl README)
