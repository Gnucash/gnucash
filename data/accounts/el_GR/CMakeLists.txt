set(account_DATA
  acctchrt_common.gnucash-xea
  acctchrt_carloan.gnucash-xea
  acctchrt_brokerage.gnucash-xea)

set_dist_list(EL_GR_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/el_GR)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/el_GR/${acct_file} COPYONLY)
endforeach()
