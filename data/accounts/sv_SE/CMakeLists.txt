set(account_DATA
	bas_2019_k1.gnucash-xea
	bas_2019_k2_grund.gnucash-xea
	acctchrt_common.gnucash-xea)

set_dist_list(SV_SE_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_SE)

foreach(acct_file ${account_DATA})
    configure_file(${acct_file} ${ACCOUNTS_BUILD_DIR}/sv_SE/${acct_file} COPYONLY)
endforeach()
