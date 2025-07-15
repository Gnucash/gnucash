
set (d3-sankey_DATA
     d3-sankey.js
)

install (FILES ${d3-sankey_DATA} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/d3-sankey)

set_dist_list (d3-sankey_DIST CMakeLists.txt ${d3-sankey_DATA})
