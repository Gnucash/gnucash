set(account_DATA acctchrt_business.gnucash-xea)

set_dist_list(ZH_TW_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/zh_TW)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/zh_TW/${acct_file} COPYONLY)
endforeach()
