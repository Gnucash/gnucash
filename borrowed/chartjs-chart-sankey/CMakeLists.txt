set(chartjs-chart-sankey_DATA
  chartjs-chart-sankey.min.js
)

install (FILES ${chartjs-chart-sankey_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/chartjs-chart-sankey)

set_dist_list(chartjs-chart-sankey_DIST CMakeLists.txt LICENSE ${chartjs-chart-sankey_DATA})
