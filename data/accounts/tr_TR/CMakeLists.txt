set(account_DATA
 acctchrt_brokerage.gnucash-xea
 acctchrt_carloan.gnucash-xea
 acctchrt_cdmoneymkt.gnucash-xea
 acctchrt_checkbook.gnucash-xea
 acctchrt_common.gnucash-xea
 acctchrt_fixedassets.gnucash-xea
 acctchrt_homeloan.gnucash-xea
 acctchrt_TEKDUZ.gnucash-xea)

set_dist_list(TR_TR_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/tr_TR)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/tr_TR/${acct_file} COPYONLY)
endforeach()
