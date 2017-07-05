# CMakeLists.txt for src/gnc-module
ADD_SUBDIRECTORY(test)

IF (BUILDING_FROM_VCS)
  # Command to generate the swig-engine.c wrapper file
  SET (SWIG_GNC_MODULE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-gnc-module.c)
  GNC_ADD_SWIG_COMMAND (swig-gnc-module-c ${SWIG_GNC_MODULE_C} ${CMAKE_CURRENT_SOURCE_DIR}/gnc-module.i)
ELSE()
  SET (SWIG_GNC_MODULE_C swig-gnc-module.c)
ENDIF()

SET (gnc_module_SOURCES gnc-module.c)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${gnc_module_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (gnc_module_HEADERS
  gnc-module.h
  gnc-module-api.h
)

ADD_LIBRARY	(gnc-module
  ${gnc_module_SOURCES}
  ${gnc_module_HEADERS}
  ${SWIG_GNC_MODULE_C}
)

TARGET_LINK_LIBRARIES(gnc-module gnc-qof ${GUILE_LDFLAGS} ${GMODULE_LDFLAGS} ${GLIB2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gnc-module PRIVATE -DG_LOG_DOMAIN=\"gnc.module\")

TARGET_INCLUDE_DIRECTORIES (gnc-module
    PRIVATE ${GUILE_INCLUDE_DIRS}
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR}
)

INSTALL(TARGETS gnc-module
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin)

INSTALL(FILES ${gnc_module_HEADERS} DESTINATION include/gnucash)

# --- Compile Scheme file(s) ---

SET (gnc_module_SCHEME gnc-module.scm)

SET(GUILE_OUTPUT_DIR   gnucash)
SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    "")
SET(GUILE_LIBRARY_DIRS "")
SET(GUILE_DEPENDS      gnc-module)

GNC_ADD_SCHEME_TARGETS(scm-gnc-module
  "${gnc_module_SCHEME}"
  ${GUILE_OUTPUT_DIR} 
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

SET_LOCAL_DIST(gnc_module_DIST_local CMakeLists.txt Makefile.am ${gnc_module_SOURCES} ${gnc_module_HEADERS}
        gnc-module.i gnc-module.scm README)
SET(gnc_module_DIST ${gnc_module_DIST_local} ${test_gnc_module_DIST} PARENT_SCOPE)