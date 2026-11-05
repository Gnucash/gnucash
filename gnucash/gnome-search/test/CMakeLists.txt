set(search_currency_test_INCLUDES
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-search
)
set(search_currency_test_LIBS gnc-gnome-search gtest)
gnc_add_test(test-search-currency test-search-currency.cpp
             search_currency_test_INCLUDES search_currency_test_LIBS)

set_dist_list(test_gnome_search_DIST CMakeLists.txt test-search-currency.cpp)
