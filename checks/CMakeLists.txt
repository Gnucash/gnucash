SET(checks_DATA
  deluxe.chk
  liberty.chk
  quicken.chk
  quicken_wallet.chk
  voucher.chk
  quicken_check_21.chk
  quicken_3part.chk
)

INSTALL(FILES ${checks_DATA} DESTINATION ${CMAKE_INSTALL_PREFIX}/share/gnucash/checks)

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY ${checks_DATA} DESTINATION ${DATADIR_BUILD}/gnucash/checks)
ENDIF()