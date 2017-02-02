ADD_SUBDIRECTORY(business-gnome)
ADD_SUBDIRECTORY(business-ledger)

SET_LOCAL_DIST(business_DIST_local CMakeLists.txt Makefile.am)
SET(business_DIST ${business_DIST_local} ${business_gnome_DIST} ${business_ledger_DIST} PARENT_SCOPE)