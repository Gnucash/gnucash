set(account_DATA acctchrt_business.gnucash-xea)

set_dist_list(ZH_HK_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/zh_HK)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/zh_HK)
