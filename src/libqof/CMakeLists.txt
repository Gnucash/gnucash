# CMakeLists.txt for src/libqof

# EXPERIMENTAL! This is just a trial of how far we can get for a cmake
# build system.

ADD_DEFINITIONS (-DG_LOG_DOMAIN=\"qof\")

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/../lib/libc) # for strptime.h


# Workaround to create a very simple gncla-dir.h file
FILE (WRITE ${CMAKE_CURRENT_BINARY_DIR}/qofla-dir.h "
#define QOF_LIB_DIR \"${CMAKE_INSTALL_PREFIX}/lib/gnucash\"
")
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for qofla-dir.h

SET (libgnc_qof_SOURCES
   qof/gnc-date.c        
   qof/gnc-numeric.c     
   qof/guid.c            
   qof/kvp_frame.c       
   qof/kvp-util.c        
   qof/md5.c             
   qof/qofbackend.c      
   qof/qofclass.c        
   qof/qofchoice.c       
   qof/qofid.c           
   qof/qofinstance.c     
   qof/qofquery.c        
   qof/qofbook.c         
   qof/qofevent.c        
   qof/qoflog.c          
   qof/qofobject.c       
   qof/qofquerycore.c    
   qof/qofreference.c    
   qof/qofutil.c         
   qof/qofsession.c      
   qof/qofbookmerge.c
)
IF (WIN32)
  SET (libgnc_qof_SOURCES ${libgnc_qof_SOURCES} qof/qof-win32.c)
ENDIF (WIN32)

SET (qofinclude_HEADERS
   qof/gnc-date.h        
   qof/gnc-date-p.h      
   qof/gnc-numeric.h     
   qof/guid.h            
   qof/kvp_frame.h       
   qof/kvp-util.h        
   qof/kvp-util-p.h      
   qof/qof.h             
   qof/qofbackend.h      
   qof/qofbackend-p.h    
   qof/qofclass.h        
   qof/qofchoice.h       
   qof/qofevent.h        
   qof/qofgobj.h         
   qof/qofid.h           
   qof/qofid-p.h         
   qof/qofinstance-p.h   
   qof/qofinstance.h     
   qof/qofquery.h        
   qof/qofbook.h         
   qof/qofbookslots.h    
   qof/qoflog.h          
   qof/qofobject.h       
   qof/qofquerycore.h    
   qof/qofreference.h    
   qof/qofsession.h      
   qof/qofsql.h          
   qof/qofutil.h         
   qof/qofbookmerge.h    
   qof/qof-gobject.h
)

ADD_LIBRARY	(qof
  ${libgnc_qof_SOURCES}
  ${qofinclude_HEADERS}
  )

TARGET_LINK_LIBRARIES (qof ${GTK_LIBRARIES})

# libgnc_qof_la_LIBADD= $(REGEX_LIBS)

#nodist_qofinclude_HEADERS = \
#   qofla-dir.h     

#noinst_HEADERS = \
#   md5.h  \
#   qofclass-p.h  \
#   qofmath128.h  \
#   qofquery-p.h  \
#   qofquery-deserial.h \
#   qofquery-serialize.h \
#   qofbook-p.h  \
#   qofevent-p.h \
#   qofobject-p.h  \
#   qofquerycore-p.h \
#   qofsession-p.h

#EXTRA_DIST = \
#  qofmath128.c
