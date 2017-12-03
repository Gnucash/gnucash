ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(test)

SET(csv_import_remote_SOURCES
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-charmap-sel.c
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-optionmenu.c
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-glib-extras.c
)

SET(csv_import_SOURCES
  gncmod-csv-import.c
  assistant-csv-account-import.c
  assistant-csv-price-import.cpp
  assistant-csv-trans-import.cpp
  gnc-plugin-csv-import.c
  csv-account-import.c
  gnc-csv-account-map.c
  gnc-csv-gnumeric-popup.c
  gnc-csv-tokenizer.cpp
  gnc-csv-import-settings.cpp
  gnc-csv-price-import-settings.cpp
  gnc-csv-trans-import-settings.cpp
  gnc-dummy-tokenizer.cpp
  gnc-fw-tokenizer.cpp
  gnc-price-import.cpp
  gnc-price-props.cpp
  gnc-tokenizer.cpp
  gnc-trans-props.cpp
  gnc-tx-import.cpp
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${csv_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET(csv_import_remote_HEADERS
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-charmap-sel.h
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-optionmenu.h
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-glib-extras.h
)

SET(csv_import_noinst_HEADERS
  assistant-csv-account-import.h
  assistant-csv-price-import.h
  assistant-csv-trans-import.h
  gnc-plugin-csv-import.h
  csv-account-import.h
  gnc-csv-account-map.h
  gnc-csv-gnumeric-popup.h
  gnc-csv-tokenizer.hpp
  gnc-csv-import-settings.hpp
  gnc-csv-price-import-settings.hpp
  gnc-csv-trans-import-settings.hpp
  gnc-dummy-tokenizer.hpp
  gnc-fw-tokenizer.hpp
  gnc-price-import.hpp
  gnc-price-props.hpp
  gnc-tokenizer.hpp
  gnc-trans-props.hpp
  gnc-tx-import.hpp
)

ADD_LIBRARY(gncmod-csv-import ${csv_import_noinst_HEADERS}
  ${csv_import_remote_HEADERS} ${csv_import_remote_SOURCES} ${csv_import_SOURCES}
)

TARGET_LINK_LIBRARIES(
  gncmod-csv-import
  ${Boost_LIBRARIES}
  ${ICU4C_I18N_LDFLAGS}
  gncmod-generic-import
  gncmod-gnome-utils
  gncmod-app-utils
  gncmod-engine
  gnc-core-utils
  gnc-module)


TARGET_COMPILE_DEFINITIONS(gncmod-csv-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import.csv\")

TARGET_INCLUDE_DIRECTORIES(gncmod-csv-import PRIVATE
     ${ICU4C_I18N_INCLUDE_DIRS}
     ${CMAKE_SOURCE_DIR}/borrowed/goffice
)

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-csv-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-csv-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install

SET(csv_import_GLADE assistant-csv-account-import.glade
      assistant-csv-price-import.glade assistant-csv-trans-import.glade)

INSTALL(FILES ${csv_import_GLADE} DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash/gtkbuilder)

SET(csv_import_UI gnc-plugin-csv-import-ui.xml)

INSTALL(FILES ${csv_import_UI} DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash/ui)

FILE(COPY ${csv_import_UI}
  DESTINATION ${DATADIR_BUILD}/gnucash/ui)
FILE(COPY ${csv_import_GLADE}
  DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

SET_LOCAL_DIST(csv_import_DIST_local CMakeLists.txt 
        ${csv_import_SOURCES} ${csv_import_noinst_HEADERS}
        ${csv_import_GLADE} ${csv_import_UI})
SET(csv_import_DIST ${csv_import_DIST_local} ${test_csv_import_DIST} ${csv_import_gschema_DIST} PARENT_SCOPE)
