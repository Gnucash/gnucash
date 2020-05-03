set(account_DATA
  acctchrt_brokerage.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_kids.gnucash-xea
  acctchrt_otherasset.gnucash-xea
  acctchrt_otherloan.gnucash-xea
  acctchrt_kmu.gnucash-xea)

set_dist_list(DE_CH_DIST ${account_DATA} CMakeLists.txt)

install(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/de_CH)
file(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/de_CH)