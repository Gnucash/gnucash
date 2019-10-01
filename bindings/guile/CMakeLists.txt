add_subdirectory(test)

set(guile_HEADERS
    gnc-guile-utils.h)

set(guile_SOURCES
    gnc-guile-bindings.c
    gnc-guile-utils.c)


add_library(gnucash-guile SHARED ${guile_SOURCES} ${guile_HEADERS})

target_include_directories(gnucash-guile
    PRIVATE
        ${CMAKE_SOURCE_DIR}/common
        ${CMAKE_BINARY_DIR}/common
        ${GLIB2_INCLUDE_DIRS}
        ${GUILE_INCLUDE_DIRS}
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(gnucash-guile
    ${GUILE_LDFLAGS}
    ${GLIB2_LDFLAGS}
    ${GOBJECT_LDFLAGS}
    ${GTK_MAC_LDFLAGS})

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


