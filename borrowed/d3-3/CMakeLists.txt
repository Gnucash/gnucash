
set(d3-3_DATA
    d3.v3.js
    d3.v3.min.js
)

install (FILES ${d3-3_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/d3-3)

set_dist_list(d3-3_DIST CMakeLists.txt ${d3-3_DATA})
