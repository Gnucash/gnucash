
SET (gnc_business_ledger_SOURCES
  gncEntryLedger.c
  gncEntryLedgerControl.c
  gncEntryLedgerDisplay.c
  gncEntryLedgerLayout.c
  gncEntryLedgerLoad.c
  gncEntryLedgerModel.c
)

SET (gnc_business_ledger_noinst_HEADERS
  gncEntryLedger.h
  gncEntryLedgerP.h
  gncEntryLedgerControl.h
  gncEntryLedgerLayout.h
  gncEntryLedgerModel.h
)

ADD_LIBRARY (gnc-business-ledger
  ${gnc_business_ledger_SOURCES}
  ${gnc_business_ledger_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gnc-business-ledger gncmod-ledger-core  gncmod-register-core gncmod-engine gncmod-gnome-utils
                            gnc-core-utils ${GTK2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gnc-business-ledger PRIVATE -DG_LOG_DOMAIN=\"gnc.business.ledger\")

TARGET_INCLUDE_DIRECTORIES (gnc-business-ledger PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

INSTALL(TARGETS gnc-business-ledger
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin)
# No headers to install

SET_DIST_LIST(business_ledger_DIST CMakeLists.txt Makefile.am ${gnc_business_ledger_SOURCES} ${gnc_business_ledger_noinst_HEADERS})
