SET(GUILE_DEPENDS      scm-core-utils scm-gnc-module)


GNC_ADD_SCHEME_TARGETS(scm-scm
  "utilities.scm"
  gnucash
  "${GUILE_DEPENDS}"
  FALSE
)

# depends on main.scm
GNC_ADD_SCHEME_TARGETS(price-quotes
  price-quotes.scm
  gnucash
  "scm-scm;scm-gnome-utils;scm-app-utils"
  FALSE)


SET_LOCAL_DIST(scm_DIST_local CMakeLists.txt utilities.scm price-quotes.scm)
SET(scm_DIST ${scm_DIST_local} ${scm_gnumeric_DIST} PARENT_SCOPE)


