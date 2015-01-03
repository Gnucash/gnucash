# CMakeLists.txt for src/libqof

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"qof\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}/.. ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/lib/libc) # for strptime.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/src) # for platform.h

SET (libgnc_qof_SOURCES
   qof/gnc-date.cpp
   qof/gnc-int128.cpp
   qof/gnc-numeric.cpp
   qof/gnc-rational.cpp
   qof/guid.cpp
   qof/kvp-util.cpp
   qof/kvp_frame.cpp
   qof/kvp-value.cpp
   qof/qofbackend.cpp
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
  SET (libgnc_qof_SOURCES ${libgnc_qof_SOURCES}
    qof/qof-win32.c
	../../lib/libc/strptime.c
	../../lib/libc/localtime_r.c
	../../lib/libc/gmtime_r.c
  )
ENDIF (WIN32)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${libgnc_qof_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (libgnc_qof_HEADERS
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
   qof/qofsession.h
   qof/qofutil.h
   qof/qof-gobject.h
   qof/qof-string-cache.h
)

ADD_LIBRARY	(qof
  ${libgnc_qof_SOURCES}
  ${libgnc_qof_HEADERS}
  )
