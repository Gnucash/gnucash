SET(account_DATA
  acctchrt_checkbook.gnucash-xea
  acctchrt_full.gnucash-xea
  acctchrt_rgs_1.1.gnucash-xea)

SET_DIST_LIST(NL_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/nl)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/nl)
