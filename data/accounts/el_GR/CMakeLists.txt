SET(account_DATA
  acctchrt_common.gnucash-xea
  acctchrt_carloan.gnucash-xea
  acctchrt_brokerage.gnucash-xea)

SET_DIST_LIST(EL_GR_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/el_GR)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/el_GR)