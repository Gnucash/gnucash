SET(account_DATA
    acctchrt_common.gnucash-xea
    acctchrt_sbr-xbrl.gnucash-xea
    acctchrt_rf.gnucash-xea)

SET_DIST_LIST(SV_AX_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_AX)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/sv_AX)

