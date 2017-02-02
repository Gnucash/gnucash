# CMakeLists.txt for src/optional/gtkmm
#ADD_SUBDIRECTORY(gncmm)
ADD_SUBDIRECTORY(test)


ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"gnc.gtkmm\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${GLIBMM_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${GUILE_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src ) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/gnc-module) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/engine) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src/backend/xml)
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/src/engine) # for swig-runtime.h

SET (libgncmod_gtkmm_HEADERS
  gncmm/Account.hpp
  gncmm/Book.hpp
  gncmm/Commodity.hpp
  gncmm/GncInstance.hpp
  gncmm/Numeric.hpp
  gncmm/Split.hpp
  gncmm/Transaction.hpp
  gncmm/private/Account_p.hpp
  gncmm/private/Book_p.hpp
  gncmm/private/Commodity_p.hpp
  gncmm/private/GncInstance_p.hpp
  gncmm/private/Split_p.hpp
  gncmm/private/Transaction_p.hpp
  gncmm/wrap_init.hpp
)

SET (libgncmod_gtkmm_SOURCES
  gncmm/Account.cpp
  gncmm/Book.cpp
  gncmm/Commodity.cpp
  gncmm/GncInstance.cpp
  gncmm/Numeric.cpp
  gncmm/Split.cpp
  gncmm/Transaction.cpp
  gncmm/wrap_init.cpp
)

SET(gnc_plugin_SOURCES gnc-plugin-gtkmm.cpp gncmod-gtkmm.cpp)
SET(gnc_plugin_noinst_SOURCES gnc-plugin-gtkmm.hpp)
SET(gnc_plugin_UI gnc-plugin-gtkmm-ui.xml)

IF (WITH_CUTECASH)
  ADD_LIBRARY (libgncmod-gtkmm
    ${libgncmod_gtkmm_SOURCES}
    ${libgncmod_gtkmm_HEADERS}
  )
ENDIF()

SET_LOCAL_DIST(gtkmm_DIST_local CMakeLists.txt Makefile.am
        ${libgncmod_gtkmm_SOURCES} ${libgncmod_gtkmm_HEADERS}
        ${gnc_plugin_SOURCES} ${gnc_plugin_noinst_SOURCES} ${gnc_plugin_UI})
SET(gtkmm_DIST ${gtkmm_DIST_local} ${test_gtkmm_DIST} PARENT_SCOPE)
