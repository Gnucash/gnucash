set(account_DATA
	bas_2019_k1.gnucash-xea
	bas_2019_k2_grund.gnucash-xea
	acctchrt_common.gnucash-xea)

set_dist_list(SV_SE_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_SE)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/sv_SE)
