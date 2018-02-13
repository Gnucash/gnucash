

SET(account_DATA
  acctchrt_gstindia.gnucash-xea)
# Fixme: do we want to copy this files, too?
#  acctchrt_brokerage.gnucash-xea
#  acctchrt_business.gnucash-xea
#  acctchrt_carloan.gnucash-xea
#  acctchrt_cdmoneymkt.gnucash-xea
#  acctchrt_checkbook.gnucash-xea
#  acctchrt_childcare.gnucash-xea
#  acctchrt_common.gnucash-xea
#  acctchrt_eduloan.gnucash-xea
#  acctchrt_fixedassets.gnucash-xea
#  acctchrt_homeloan.gnucash-xea
#  acctchrt_homeown.gnucash-xea
#  acctchrt_otherloan.gnucash-xea
#  acctchrt_renter.gnucash-xea
#  acctchrt_retiremt.gnucash-xea
#  acctchrt_spouseinc.gnucash-xea
#  acctchrt_spouseretire.gnucash-xea)

SET_DIST_LIST(EN_IN_DIST ${account_DATA}
#  acctchrt_full.gnucash-xea
  Makefile.am CMakeLists.txt)

INSTALL(FILES ${account_DATA} DESTINATION ${ACCOUNTS_INSTALL_DIR}/en_IN)
FILE(COPY ${account_DATA} DESTINATION ${ACCOUNTS_BUILD_DIR}/en_IN)

