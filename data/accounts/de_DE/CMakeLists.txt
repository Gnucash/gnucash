SET(account_DATA
  acctchrt_auto.gnucash-xea
  acctchrt_autoloan.gnucash-xea
  acctchrt_brokerage.gnucash-xea
  acctchrt_common.gnucash-xea
  acctchrt_houseown.gnucash-xea
  acctchrt_investment.gnucash-xea
  acctchrt_kids.gnucash-xea
  acctchrt_otherasset.gnucash-xea
  acctchrt_otherloan.gnucash-xea
  acctchrt_studium.gnucash-xea
  acctchrt_skr03.gnucash-xea
  acctchrt_wohnungsw.gnucash-xea
  acctchrt_skr04.gnucash-xea
  acctchrt_skr49.gnucash-xea)

SET_DIST_LIST(DE_DE_DIST ${account_DATA} acctchrt_full.gnucash-xea CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/de_DE)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/de_DE)