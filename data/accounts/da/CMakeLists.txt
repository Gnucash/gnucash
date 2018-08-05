set(account_DATA
  acctchrt_car.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_homeloan.gnucash-xea
  acctchrt_homeown.gnucash-xea)

set_dist_list(DA_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/da)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/da)
