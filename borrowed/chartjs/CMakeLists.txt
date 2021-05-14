
set(chartjs_DATA
  package/dist/chart.js
)

file(GLOB_RECURSE chartjs_NOINST_DATA
    RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    samples Chart*)

list(REMOVE_ITEM chartjs_NOINST_DATA ${chartjs_DATA})

install (FILES ${chartjs_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/chartjs)

set_dist_list(chartjs_DIST CMakeLists.txt ${chartjs_DATA} ${chartjs_NOINST_DATA})
