SET(account_DATA
  acctchrt_brokerage.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_kids.gnucash-xea
  acctchrt_otherasset.gnucash-xea
  acctchrt_otherloan.gnucash-xea
  acctchrt_chkmu.gnucash-xea)

SET_DIST_LIST(DE_CH_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/de_CH)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/de_CH)