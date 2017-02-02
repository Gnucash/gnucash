# CMakeLists.txt for src/libqof

ADD_SUBDIRECTORY(qof)

SET_LOCAL_DIST(libqof_DIST_local CMakeLists.txt Makefile.am)
SET(libqof_DIST ${libqof_DIST_local} ${qof_DIST} PARENT_SCOPE)