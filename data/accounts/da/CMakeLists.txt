SET(account_DATA
  acctchrt_car.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_homeloan.gnucash-xea
  acctchrt_homeown.gnucash-xea)

SET_DIST_LIST(DA_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/da)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/da)
