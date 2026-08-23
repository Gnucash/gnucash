set(chartjs-4_DATA
  chart.umd.js
)

install (FILES ${chartjs-4_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/chartjs-4)

set_dist_list(chartjs-4_DIST CMakeLists.txt LICENSE.md ${chartjs-4_DATA})
