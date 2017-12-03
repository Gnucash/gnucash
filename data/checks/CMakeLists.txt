SET(checks_DATA
  deluxe.chk
  liberty.chk
  quicken.chk
  quicken_wallet.chk
  voucher.chk
  quicken_check_21.chk
  quicken_3part.chk
)

INSTALL(FILES ${checks_DATA} DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash/checks)

FILE(COPY ${checks_DATA} DESTINATION ${DATADIR_BUILD}/gnucash/checks)

SET_DIST_LIST(checks_DIST CMakeLists.txt ${checks_DATA})
