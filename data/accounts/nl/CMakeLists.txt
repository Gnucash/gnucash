set(account_DATA
  acctchrt_checkbook.gnucash-xea
  acctchrt_full.gnucash-xea
  acctchrt_rgs_1.1.gnucash-xea)

set_dist_list(NL_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/nl)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/nl)
