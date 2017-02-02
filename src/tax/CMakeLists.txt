
ADD_SUBDIRECTORY(us)

SET_LOCAL_DIST(tax_DIST_local CMakeLists.txt Makefile.am)
SET(tax_DIST ${tax_DIST_local} ${tax_us_DIST} PARENT_SCOPE)