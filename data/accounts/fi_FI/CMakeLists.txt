SET(account_DATA
  acctchrt_common.gnucash-xea
  acctchrt_sbr-xbrl.gnucash-xea
  acctchrt_ry.gnucash-xea)

SET_DIST_LIST(FI_FI_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/fi_FI)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/fi_FI)
