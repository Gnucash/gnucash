add_subdirectory(test)

set(csv_import_remote_SOURCES
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-charmap-sel.c
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-optionmenu.c
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-glib-extras.c
)

set(csv_import_SOURCES
  assistant-csv-account-import.c
  assistant-csv-price-import.cpp
  assistant-csv-trans-import.cpp
  gnc-plugin-csv-import.c
  csv-account-import.c
  gnc-csv-gnumeric-popup.c
  gnc-imp-props-price.cpp
  gnc-imp-props-tx.cpp
  gnc-imp-settings-csv.cpp
  gnc-imp-settings-csv-price.cpp
  gnc-imp-settings-csv-tx.cpp
  gnc-import-price.cpp
  gnc-import-tx.cpp
  gnc-tokenizer.cpp
  gnc-tokenizer-csv.cpp
  gnc-tokenizer-dummy.cpp
  gnc-tokenizer-fw.cpp
)

# Add dependency on config.h
set_source_files_properties (${csv_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(csv_import_remote_HEADERS
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-charmap-sel.h
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-optionmenu.h
  ${CMAKE_SOURCE_DIR}/borrowed/goffice/go-glib-extras.h
)

set(csv_import_noinst_HEADERS
  assistant-csv-account-import.h
  assistant-csv-price-import.h
  assistant-csv-trans-import.h
  gnc-plugin-csv-import.h
  csv-account-import.h
  gnc-csv-gnumeric-popup.h
  gnc-imp-props-price.hpp
  gnc-imp-props-tx.hpp
  gnc-imp-settings-csv.hpp
  gnc-imp-settings-csv-price.hpp
  gnc-imp-settings-csv-tx.hpp
  gnc-import-price.hpp
  gnc-import-tx.hpp
  gnc-tokenizer.hpp
  gnc-tokenizer-csv.hpp
  gnc-tokenizer-dummy.hpp
  gnc-tokenizer-fw.hpp
)

add_library(gnc-csv-import ${csv_import_noinst_HEADERS}
  ${csv_import_remote_HEADERS} ${csv_import_remote_SOURCES} ${csv_import_SOURCES}
)

target_link_libraries(
  gnc-csv-import
  ${Boost_LIBRARIES}
  ${ICU4C_I18N_LDFLAGS}
  gnc-generic-import
  gnc-gnome-utils
  gnc-app-utils
  gnc-engine
  gnc-core-utils)


target_compile_definitions(gnc-csv-import PRIVATE -DG_LOG_DOMAIN=\"gnc.import.csv\")

target_include_directories(gnc-csv-import
    PRIVATE
        ${ICU4C_I18N_INCLUDE_DIRS}
        ${CMAKE_SOURCE_DIR}/borrowed/goffice
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

if (APPLE)
  set_target_properties (gnc-csv-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

if (COVERAGE)
  add_coverage_target(gnc-csv-import)
endif()

install(TARGETS gnc-csv-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install

set_local_dist(csv_import_DIST_local CMakeLists.txt
        ${csv_import_SOURCES} ${csv_import_noinst_HEADERS})
set(csv_import_DIST ${csv_import_DIST_local} ${test_csv_import_DIST} PARENT_SCOPE)
