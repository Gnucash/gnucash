

SET(examples_DATA
  Money95bank_fr.qif
  Money95invst_fr.qif
  Money95mfunds_fr.qif
  Money95stocks_fr.qif
  README
  README_invoice
  abc-all.qif
  abc.qif
  bogus.qif
  cbb-export.qif
  currency_tree_xml.gnucash
  every.qif
  invoice.csv
  ms-money.qif
  quicktest.qif
  swipe.qif
  taxreport.gnucash
  web.qif
)

INSTALL(FILES ${examples_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/doc/gnucash)

SET_DIST_LIST(examples_DIST ${examples_DATA} CMakeLists.txt )
