SET(account_DATA
	bas_2012.gnucash-xea
	acctchrt_common.gnucash-xea)

SET_DIST_LIST(SV_SE_DIST ${account_DATA} README.bas_2012 CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_SE)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/sv_SE)
