# CMakeLists.txt for src/libqof

ADD_SUBDIRECTORY(test)

SET (gnc_qof_HEADERS
   gnc-aqbanking-templates.h
   gnc-date-p.h
   gnc-date.h
   gnc-datetime.hpp
   gnc-timezone.hpp
   gnc-numeric.h
   gnc-numeric.hpp
   gnc-rational.hpp
   gnc-rational-rounding.hpp
   guid.h
   kvp_frame.hpp
   kvp-value.hpp
   qof.h
   qof-backend.hpp
   qofbackend.h
   qofbook.h
   qofbookslots.h
   qofchoice.h
   qofclass.h
   qofevent.h
   qofid-p.h
   qofid.h
   qofinstance-p.h
   qofinstance.h
   qoflog.h
   qofobject.h
   qofquery.h
   qofquerycore.h
   qofsession.h
   qofutil.h
   qof-gobject.h
   qof-string-cache.h
)
SET (gnc_qof_noinst_HEADERS
   gnc-backend-prov.hpp
   qofbook-p.h
   qofclass-p.h
   gnc-date-p.h
   qofevent-p.h
   gnc-int128.hpp
   qofobject-p.h
   qofquery-p.h
   qofquerycore-p.h
   qofsession.hpp
)

SET (gnc_qof_SOURCES
  gnc-aqbanking-templates.cpp
  gnc-date.cpp
  gnc-datetime.cpp
  gnc-int128.cpp
  gnc-numeric.cpp
  gnc-rational.cpp
  gnc-datetime.cpp
  gnc-timezone.cpp
  guid.cpp
  kvp_frame.cpp
  kvp-value.cpp
  qof-backend.cpp
  qofbook.cpp
  qofchoice.cpp
  qofclass.cpp
  qofevent.cpp
  qofid.cpp
  qofinstance.cpp
  qoflog.cpp
  qofobject.cpp
  qofquery.cpp
  qofquerycore.cpp
  qofsession.cpp
  qofutil.cpp
  qof-string-cache.cpp
)

IF (WIN32)
  ADD_DEFINITIONS (-DOS_WIN32)
  SET (gnc_qof_SOURCES ${gnc_qof_SOURCES}
   qof-win32.cpp
	../../../lib/libc/strptime.c
  )
ENDIF (WIN32)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${gnc_qof_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})


ADD_LIBRARY(gnc-qof
  ${gnc_qof_SOURCES}
  ${gnc_qof_HEADERS}
  ${gnc_qof_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gnc-qof ${Boost_DATE_TIME_LIBRARIES}  ${Boost_REGEX_LIBRARIES} ${REGEX_LDFLAGS} ${GMODULE_LDFLAGS} ${GLIB2_LDFLAGS} ${GOBJECT_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gnc-qof PRIVATE -DG_LOG_DOMAIN=\"qof\")

TARGET_INCLUDE_DIRECTORIES (gnc-qof PUBLIC
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src # for platform.h
  ${CMAKE_CURRENT_SOURCE_DIR}/qof
  ${GLIB2_INCLUDE_DIRS}
  ${LIBINTL_INCLUDE_PATH}
  ${REGEX_INCLUDE_PATH}
  ${CMAKE_SOURCE_DIR}/lib/libc # for strptime.h
)

INSTALL(TARGETS gnc-qof
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin)
INSTALL(FILES ${gnc_qof_HEADERS} DESTINATION include/gnucash)

SET(qof_test_HEADERS
    kvp_frame.hpp
    qofbook.h
    qofinstance.h
    qofobject.h
    qofsession.h
)

INSTALL(FILES ${qof_test_HEADERS} DESTINATION libexec/gnucash/src/libqof/qof/test)
SET_LOCAL_DIST(qof_DIST_local CMakeLists.txt Makefile.am ${gnc_qof_EXTRA_DIST}
  ${gnc_qof_HEADERS} ${gnc_qof_noinst_HEADERS} ${gnc_qof_SOURCES})
SET(qof_DIST ${qof_DIST_local} ${test_qof_DIST} PARENT_SCOPE) 
