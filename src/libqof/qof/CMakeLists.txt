ADD_SUBDIRECTORY(test)

SET (gnc_qof_HEADERS
  gnc-date-p.h
  gnc-date.h
  gnc-numeric.h
  guid.h
  kvp-util-p.h
  kvp-util.h
  kvp_frame.h
  qof.h
  qofbackend-p.h
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
  qofreference.h
  qofsession.h
  qof-string-cache.h
  qofutil.h
  qof-gobject.h
)
SET (gnc_qof_noinst_HEADERS
  md5.h
  qofbook-p.h
  qofclass-p.h
  qofevent-p.h
  qofmath128-p.h
  qofobject-p.h
  qofquery-p.h
  qofquerycore-p.h
  qofsession-p.h
)

SET (gnc_qof_SOURCES
  md5.c
  gnc-date.c
  gnc-numeric.c
  guid.c
  kvp-util.c
  kvp_frame.c
  qofbackend.c
  qofbook.c
  qofchoice.c
  qofclass.c
  qofevent.c
  qofid.c
  qofinstance.c
  qoflog.c
  qofobject.c
  qofquery.c
  qofquerycore.c
  qofreference.c
  qofsession.c
  qof-string-cache.c
  qofutil.c
)

IF (WIN32)
  ADD_DEFINITIONS (-DOS_WIN32)
  SET (gnc_qof_SOURCES ${gnc_qof_SOURCES}
    qof-win32.c
	../../../lib/libc/strptime.c
	../../../lib/libc/localtime_r.c
	../../../lib/libc/gmtime_r.c
  )
ENDIF (WIN32)

SET(gnc_qof_EXTRA_DIST
		qofmath128.c
		qof-win32.c
		)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${gnc_qof_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

# Must compile md5.c without strict aliasing, otherwise function md5_finish_ctx
# gets "dereferencing type-punned pointer will break strict-aliasing rules"
SET_SOURCE_FILES_PROPERTIES (md5.c PROPERTIES COMPILE_FLAGS "-fno-strict-aliasing")

ADD_LIBRARY	(gnc-qof
  ${gnc_qof_SOURCES}
  ${gnc_qof_HEADERS}
  ${gnc_qof_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gnc-qof ${Boost_DATE_TIME_LIBRARIES} ${REGEX_LDFLAGS} ${GMODULE_LDFLAGS} ${GLIB2_LDFLAGS} ${GOBJECT_LDFLAGS} -lm)

TARGET_COMPILE_DEFINITIONS (gnc-qof PRIVATE -DG_LOG_DOMAIN=\"qof\")

TARGET_INCLUDE_DIRECTORIES (gnc-qof PUBLIC
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src # for platform.h
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${GLIB2_INCLUDE_DIRS}
  ${LIBINTL_INCLUDE_PATH}
  ${REGEX_INCLUDE_PATH}
  ${CMAKE_SOURCE_DIR}/lib/libc # for strptime.h
)

INSTALL(TARGETS gnc-qof
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin)

INSTALL(FILES ${gnc_qof_HEADERS} DESTINATION include/gnucash/qof)

SET(qof_test_HEADERS
    kvp_frame.h
    qofbook.h
    qofinstance.h
    qofobject.h
    qofsession.h
)

INSTALL(FILES ${qof_test_HEADERS} DESTINATION libexec/gnucash/src/libqof/qof/test)

SET_LOCAL_DIST(qof_DIST_local CMakeLists.txt Makefile.am ${gnc_qof_EXTRA_DIST} ${gnc_qof_HEADERS} ${gnc_qof_noinst_HEADERS} ${gnc_qof_SOURCES})
SET(qof_DIST ${qof_DIST_local} ${test_qof_DIST} PARENT_SCOPE)