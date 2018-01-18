
SET(dist_account_DATA
  acctchrt_auto.gnucash-xea
  acctchrt_autoloan.gnucash-xea
  acctchrt_brokerage.gnucash-xea
  acctchrt_business.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_houseown.gnucash-xea
  acctchrt_investment.gnucash-xea
  acctchrt_kids.gnucash-xea)

SET_DIST_LIST(DE_AT_DIST ${dist_account_DATA} CMakeLists.txt)

INSTALL(FILES ${dist_account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/de_AT)
FILE(COPY ${dist_account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/de_AT)
