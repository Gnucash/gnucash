set(checks_DATA
  deluxe.chk
  liberty.chk
  quicken.chk
  quicken_wallet.chk
  voucher.chk
  quicken_check_21.chk
  quicken_3part.chk
)

install(FILES ${checks_DATA} DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash/checks)

foreach(check_file ${checks_DATA})
    configure_file(${check_file} ${DATADIR_BUILD}/gnucash/checks/${check_file} COPYONLY)
endforeach()

set_dist_list(checks_DIST CMakeLists.txt ${checks_DATA})
