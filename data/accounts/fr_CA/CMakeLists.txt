SET(account_DATA
 acctchrt_actifsfixes.gnucash-xea
 acctchrt_automobile.gnucash-xea
 acctchrt_basecommune.gnucash-xea
 acctchrt_cdmarchemon.gnucash-xea
 acctchrt_chequier.gnucash-xea
 acctchrt_conjointretraite.gnucash-xea
 acctchrt_conjointrev.gnucash-xea
 acctchrt_courtage.gnucash-xea
 acctchrt_etudeemprunt.gnucash-xea
 acctchrt_garderie.gnucash-xea
 acctchrt_locataire.gnucash-xea
 acctchrt_proprietaire.gnucash-xea
 acctchrt_retraite.gnucash-xea
 acctchrt_revenus.gnucash-xea)

SET_DIST_LIST(FR_CA_DIST ${account_DATA} CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/fr_CA)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/fr_CA)

