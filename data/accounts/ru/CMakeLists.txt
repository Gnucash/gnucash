SET(account_DATA
  acctchrt_common.gnucash-xea
  acctchrt_homeloan.gnucash-xea
  acctchrt_homeown.gnucash-xea
  acctchrt_renter.gnucash-xea)

SET_DIST_LIST(RU_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/ru)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/ru)

