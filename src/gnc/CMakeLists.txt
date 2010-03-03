# CMakeLists.txt for src/gnc


# A test executable
LINK_DIRECTORIES (${GLIB2_LIBRARY_DIRS}
  ${GMODULE_LIBRARY_DIRS}
  ${GOBJECT_LIBRARY_DIRS}
  ${GTHREAD_LIBRARY_DIRS}
  ${GCONF2_LIBRARY_DIRS}
)

SET (gnc_SOURCES
  Book.cpp
  Session.cpp
  main.cpp
  mainwindow.cpp
)

SET (gnc_QOBJECT_HEADERS
  AccountItemModel.hpp
  mainwindow.hpp
)
SET (gnc_HEADERS ${gnc_QOBJECT_HEADERS}
  Account.hpp
  Book.hpp
  ScopedPointer.hpp
  Session.hpp
  WeakPointer.hpp
)

SET (gnc_FORMS
  mainwindow.ui
)

SET (gnc_RESOURCES
  gnc.qrc
)
# Generation of resource files
QT4_ADD_RESOURCES (gnc_RESOURCE ${gnc_RESOURCES})

# Generation of the foo_MOC_SRCS = moc_Class1.cxx moc_Class2.cxx
QT4_WRAP_CPP (gnc_MOC_SOURCES ${gnc_QOBJECT_HEADERS})

# Generation of the ui source files
QT4_WRAP_UI (gnc_FORMS_HEADERS ${gnc_FORMS})

INCLUDE_DIRECTORIES (${GLIB2_INCLUDE_DIRS})
INCLUDE_DIRECTORIES (${LIBINTL_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${REGEX_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${LIBGUILE_INCLUDE_PATH})
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR} ) # for config.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}) # for gnc-ui.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/libqof/qof) # for qof.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/gnc-module) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/core-utils) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/engine) # for gnc-glib-utils.h
INCLUDE_DIRECTORIES (${CMAKE_SOURCE_DIR}/backend/xml)
INCLUDE_DIRECTORIES (${CMAKE_BINARY_DIR}/engine) # for swig-runtime.h
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR}) # for ui_mainwindow.h

INCLUDE_DIRECTORIES (${QT_INCLUDES})

ADD_EXECUTABLE (cpp-main
  ${gnc_FORMS_HEADERS}
  ${gnc_HEADERS}
  ${gnc_MOC_SOURCES}
  ${gnc_RESOURCE}
  ${gnc_SOURCES}
)

TARGET_LINK_LIBRARIES (cpp-main gnc-backend-xml engine gnc-module core-utils qof)
TARGET_LINK_LIBRARIES (cpp-main ${GCONF2_LIBRARIES})
TARGET_LINK_LIBRARIES (cpp-main ${GTHREAD_LIBRARIES} ${GOBJECT_LIBRARIES} ${GMODULE_LIBRARIES} ${GLIB2_LIBRARIES})
TARGET_LINK_LIBRARIES (cpp-main ${LIBGUILE_LIBRARIES})
TARGET_LINK_LIBRARIES (cpp-main ${LIBXML2_LIBRARIES})
TARGET_LINK_LIBRARIES (cpp-main ${ZLIB_LIBRARIES})
IF (WIN32)
  TARGET_LINK_LIBRARIES (cpp-main ${LIBINTL_LIBRARY} ${REGEX_LIBRARY})
ENDIF (WIN32)

TARGET_LINK_LIBRARIES (cpp-main ${QT_QTGUI_LIBRARY} ${QT_QTCORE_LIBRARY})
