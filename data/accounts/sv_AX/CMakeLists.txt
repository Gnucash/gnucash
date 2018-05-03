set(account_DATA
    acctchrt_common.gnucash-xea
    acctchrt_sbr-xbrl.gnucash-xea
    acctchrt_rf.gnucash-xea)

set_dist_list(SV_AX_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/sv_AX)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/sv_AX)

