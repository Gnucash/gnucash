
set(gncchartjs_DATA
  Chart.bundle.min.js
  Chart.bundle.js
)

install (FILES ${gncchartjs_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/chartjs)

set_dist_list(chartjs_DIST CMakeLists.txt ${chartjs_DATA})
