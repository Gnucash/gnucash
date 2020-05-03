set(account_DATA acctchrt_business.gnucash-xea)

set_dist_list(LT_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/lt)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/lt)
