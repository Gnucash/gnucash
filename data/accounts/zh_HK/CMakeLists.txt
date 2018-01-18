SET(account_DATA acctchrt_business.gnucash-xea)

SET_DIST_LIST(ZH_HK_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/zh_HK)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/zh_HK)
