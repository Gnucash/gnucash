set(account_DATA
  acctchrt_common.gnucash-xea
  acctchrt_carloan.gnucash-xea
  acctchrt_brokerage.gnucash-xea)

set_dist_list(EL_GR_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/el_GR)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/el_GR)