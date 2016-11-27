# CMakeLists.txt for src/libqof

ADD_SUBDIRECTORY(qof/test)

SET (gnc_qof_HEADERS
    qof/gnc-aqbanking-templates.h
    qof/gnc-date-p.h
    qof/gnc-date.h
    qof/gnc-numeric.h
    qof/gnc-datetime.hpp
    qof/gnc-rational.hpp
    qof/gnc-timezone.hpp
    qof/guid.h
    qof/kvp_frame.hpp
    qof/kvp-value.hpp
    qof/qof.h
    qof/qof-backend.hpp
    qof/qofbackend.h
    qof/qofbook.h
    qof/qofbookslots.h
    qof/qofchoice.h
    qof/qofclass.h
    qof/qofevent.h
    qof/qofid-p.h
    qof/qofid.h
    qof/qofinstance-p.h
    qof/qofinstance.h
    qof/qoflog.h
    qof/qofobject.h
    qof/qofquery.h
    qof/qofquerycore.h
    qof/qofsession.h
    qof/qofutil.h
    qof/qof-gobject.h
    qof/qof-string-cache.h
)
SET (gnc_qof_noinst_HEADERS
    qof/gnc-backend-prov.hpp
    qof/qofbook-p.h
    qof/qofclass-p.h
    qof/gnc-date-p.h
    qof/qofevent-p.h
    qof/gnc-int128.hpp
    qof/qofobject-p.h
    qof/qofquery-p.h
    qof/qofquerycore-p.h
    qof/qofsession.hpp
)

SET (gnc_qof_SOURCES
   qof/gnc-aqbanking-templates.cpp
   qof/gnc-date.cpp
   qof/gnc-datetime.cpp
   qof/gnc-int128.cpp
   qof/gnc-numeric.cpp
   qof/gnc-rational.cpp
   qof/gnc-datetime.cpp
   qof/gnc-timezone.cpp
   qof/guid.cpp
   qof/kvp_frame.cpp
   qof/kvp-value.cpp
   qof/qof-backend.cpp
   qof/qofbook.cpp
   qof/qofchoice.cpp
   qof/qofclass.cpp
   qof/qofevent.cpp
   qof/qofid.cpp
   qof/qofinstance.cpp
   qof/qoflog.cpp
   qof/qofobject.cpp
   qof/qofquery.cpp
   qof/qofquerycore.cpp
   qof/qofsession.cpp
   qof/qofutil.cpp
   qof/qof-string-cache.cpp
)

IF (WIN32)
  ADD_DEFINITIONS (-DOS_WIN32)
  SET (gnc_qof_SOURCES ${gnc_qof_SOURCES}
    qof/qof-win32.cpp
	../../lib/libc/strptime.c
  )
ENDIF (WIN32)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${gnc_qof_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})


ADD_LIBRARY	(gnc-qof
  ${gnc_qof_SOURCES}
  ${gnc_qof_HEADERS}
  ${gnc_qof_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gnc-qof ${Boost_DATE_TIME_LIBRARIES} ${REGEX_LDFLAGS} ${GMODULE_LDFLAGS} ${GLIB2_LDFLAGS} ${GOBJECT_LDFLAGS})

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
    qof/kvp_frame.hpp
    qof/qofbook.h
    qof/qofinstance.h
    qof/qofobject.h
    qof/qofsession.h
)

INSTALL(FILES ${qof_test_HEADERS} DESTINATION libexec/gnucash/src/libqof/qof/test)
