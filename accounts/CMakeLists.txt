SET(accounts_SUBDIRS
  C
  cs
  da
  de_AT
  de_CH
  de_DE
  el_GR
  en_GB
  es_ES
  es_MX
  fi_FI
  fr_CA
  fr_CH
  fr_FR
  hu_HU
  it
#  ja
  ko
  lt
  lv
#  nb
#  nl
  pl
  pt_BR
  pt_PT
  ru
  sk
  sv_AX
  sv_FI
  sv_SE
  tr_TR
  zh_CN
  zh_HK
  zh_TW
)


# Most of the account subdirectories that have acctchrt_full.gnucash-xea do not install it. Don't know why.
# But these subdirectories do.
SET(accounts_fullcharts_SUBDIRS ja nb nl)

FOREACH(dir ${accounts_SUBDIRS})
  INSTALL(DIRECTORY ${dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/share/gnucash/accounts
                           PATTERN Makefile.* EXCLUDE
                           PATTERN acctchrt_full.gnucash-xea EXCLUDE
                           PATTERN README.bas_2012 EXCLUDE)
  IF (GNC_BUILD_AS_INSTALL)
    FILE(COPY ${dir} DESTINATION ${DATADIR_BUILD}/gnucash/accounts
      PATTERN Makefile.* EXCLUDE
      PATTERN acctchrt_full.gnucash-xea EXCLUDE
      PATTERN README.bas_2012 EXCLUDE
    )
  ENDIF()
ENDFOREACH(dir)

FOREACH(dir ${accounts_fullcharts_SUBDIRS})
  INSTALL(DIRECTORY ${dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/share/gnucash/accounts
      PATTERN Makefile.* EXCLUDE
      PATTERN README.bas_2012 EXCLUDE)
  IF (GNC_BUILD_AS_INSTALL)
    FILE(COPY ${dir} DESTINATION ${DATADIR_BUILD}/gnucash/accounts
      PATTERN Makefile.* EXCLUDE
      PATTERN README.bas_2012 EXCLUDE
    )
  ENDIF()
ENDFOREACH(dir)