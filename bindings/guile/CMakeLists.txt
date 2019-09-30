add_subdirectory(test)

set(guile_HEADERS )

set(guile_SOURCES
    gnc-guile-bindings.c)
set(guile_bindings_ALL_INCLUDES
    ${CMAKE_BINARY_DIR}/common
    ${CMAKE_CURRENT_SOURCE_DIR})

add_library(gnucash-guile SHARED ${guile_SOURCES} ${guile_HEADERS})
target_include_directories(gnucash-guile PUBLIC ${guile_bindings_ALL_INCLUDES})

install(TARGETS gnucash-guile
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

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


set_local_dist(guile_DIST_local CMakeLists.txt utilities.scm price-quotes.scm
    ${guile_SOURCES} ${guile_HEADERS}
)
set(guile_DIST ${guile_DIST_local} ${test_guile_DIST} PARENT_SCOPE)


