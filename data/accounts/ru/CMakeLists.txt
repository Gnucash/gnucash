set(account_DATA
  acctchrt_auto.gnucash-xea
  acctchrt_autoloan.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_homeloan.gnucash-xea
  acctchrt_homeown.gnucash-xea
  acctchrt_kids.gnucash-xea
  acctchrt_otherloan.gnucash-xea
  acctchrt_renter.gnucash-xea)

set_dist_list(RU_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/ru)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/ru/${acct_file} COPYONLY)
endforeach()
