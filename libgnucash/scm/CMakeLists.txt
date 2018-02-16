SET(GUILE_DEPENDS      scm-core-utils scm-gnc-module)
SET(scm_scm_1_SCHEME   printf.scm string.scm utilities.scm)


GNC_ADD_SCHEME_TARGETS(scm-scm-1
  "${scm_scm_1_SCHEME}"
  gnucash
  "${GUILE_DEPENDS}"
  FALSE
)

# depends on main.scm
GNC_ADD_SCHEME_TARGETS(scm-scm-2
  price-quotes.scm
  gnucash
  scm-scm-1
  FALSE)

ADD_CUSTOM_TARGET(scm-scm ALL DEPENDS scm-scm-1 scm-scm-2)

INSTALL(FILES config DESTINATION ${CMAKE_INSTALL_FULL_SYSCONFDIR}/gnucash)

SET_LOCAL_DIST(scm_DIST_local CMakeLists.txt ${scm_scm_1_SCHEME} price-quotes.scm)
SET(scm_DIST ${scm_DIST_local} ${scm_gnumeric_DIST} PARENT_SCOPE)


