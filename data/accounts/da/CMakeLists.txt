set(account_DATA
  acctchrt_car.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_homeloan.gnucash-xea
  acctchrt_homeown.gnucash-xea)

set_dist_list(DA_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/da)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/da/${acct_file} COPYONLY)
endforeach()
