set(account_DATA
	bas_2012.gnucash-xea
	acctchrt_common.gnucash-xea)

set_dist_list(SV_SE_DIST ${account_DATA} README.bas_2012 CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_SE)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/sv_SE)
