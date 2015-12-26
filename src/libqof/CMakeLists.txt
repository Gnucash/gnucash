# CMakeLists.txt for src/libqof

SET (gnc_qof_HEADERS

  qof/gnc-date-p.h
  qof/gnc-date.h
  qof/gnc-numeric.h
  qof/guid.h
  qof/kvp-util-p.h
  qof/kvp-util.h
  qof/kvp_frame.h
  qof/qof.h
  qof/qofbackend-p.h
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
  qof/qofreference.h
  qof/qofsession.h
  qof/qof-string-cache.h
  qof/qofutil.h
  qof/qof-gobject.h

)
SET (gnc_qof_noinst_HEADERS
  qof/md5.h
  qof/qofbook-p.h
  qof/qofclass-p.h
  qof/qofevent-p.h
  qof/qofmath128-p.h
  qof/qofobject-p.h
  qof/qofquery-p.h
  qof/qofquerycore-p.h
  qof/qofsession-p.h
)

SET (gnc_qof_SOURCES
  qof/md5.c
  qof/gnc-date.c
  qof/gnc-numeric.c
  qof/guid.c
  qof/kvp-util.c
  qof/kvp_frame.c
  qof/qofbackend.c
  qof/qofbook.c
  qof/qofchoice.c
  qof/qofclass.c
  qof/qofevent.c
  qof/qofid.c
  qof/qofinstance.c
  qof/qoflog.c
  qof/qofobject.c
  qof/qofquery.c
  qof/qofquerycore.c
  qof/qofreference.c
  qof/qofsession.c
  qof/qof-string-cache.c
  qof/qofutil.c
)

IF (WIN32)
  ADD_DEFINITIONS (-DOS_WIN32)
  SET (gnc_qof_SOURCES ${gnc_qof_SOURCES}
    qof/qof-win32.c
	../../lib/libc/strptime.c
	../../lib/libc/localtime_r.c
	../../lib/libc/gmtime_r.c
  )
ENDIF (WIN32)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${gnc_qof_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

# Must compile md5.c without strict aliasing, otherwise function md5_finish_ctx
# gets "dereferencing type-punned pointer will break strict-aliasing rules"
SET_SOURCE_FILES_PROPERTIES (qof/md5.c PROPERTIES COMPILE_FLAGS "-fno-strict-aliasing")

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
    qof/kvp_frame.h
    qof/qofbook.h
    qof/qofinstance.h
    qof/qofobject.h
    qof/qofsession.h
)

INSTALL(FILES ${qof_test_HEADERS} DESTINATION libexec/gnucash/src/libqof/qof/test)
