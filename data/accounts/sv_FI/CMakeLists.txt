set(account_DATA
    acctchrt_common.gnucash-xea
    acctchrt_sbr-xbrl.gnucash-xea
    acctchrt_rf.gnucash-xea)

set_dist_list(SV_FI_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_FI)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/sv_FI/${acct_file} COPYONLY)
endforeach()
