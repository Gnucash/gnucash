set(account_DATA
  acctchrt_checkbook.gnucash-xea
  acctchrt_full.gnucash-xea
  acctchrt_rgs_1.1.gnucash-xea)

set_dist_list(NL_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/nl)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/nl/${acct_file} COPYONLY)
endforeach()
