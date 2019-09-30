add_subdirectory(test)

set(GUILE_DEPENDS      scm-core-utils scm-gnc-module gncmod-engine)


gnc_add_scheme_targets(scm-scm
  "utilities.scm"
  gnucash
  "${GUILE_DEPENDS}"
  FALSE
)

# depends on main.scm
gnc_add_scheme_targets(price-quotes
  price-quotes.scm
  gnucash
  "scm-scm;scm-app-utils"
  FALSE)


set_local_dist(guile_DIST_local CMakeLists.txt utilities.scm price-quotes.scm)
set(guile_DIST ${guile_DIST_local} ${test_guile_DIST} PARENT_SCOPE)


