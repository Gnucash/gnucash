# Copyright (c) 2015, Rob Gowin

MACRO(GNC_ADD_SCHEME_TARGETS _TARGET _SOURCE_FILES _OUTPUT_DIR_IN _GUILE_MODULES _GUILE_LOAD_DIRS _GUILE_LIBRARY_DIRS _GUILE_DEPENDS_IN MAKE_LINKS_IN)
  SET(__DEBUG FALSE)
  IF (__DEBUG)
    MESSAGE("Parameters to COMPILE_SCHEME for target ${_TARGET}")
    MESSAGE("   SOURCE_FILES: ${_SOURCE_FILES}")
    MESSAGE("   GUILE_MODULES: ${_GUILE_MODULES}")
    MESSAGE("   GUILE_LOAD_DIRS: ${_GUILE_LOAD_DIRS}")
    MESSAGE("   GUILE_LIBRARY_DIRS: ${_GUILE_LIBRARY_DIRS}")
    MESSAGE("   GUILE_DEPENDS_IN: ${_GUILE_DEPENDS_IN}")
  ENDIF(__DEBUG)
  SET(_CMD "create_symlink")
  IF(WIN32)
    SET(_CMD "copy")
  ENDIF(WIN32)
  SET(MAKE_LINKS ${MAKE_LINKS_IN})
  SET(_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/${_OUTPUT_DIR_IN})
  IF(GNC_BUILD_AS_INSTALL)
    SET(_OUTPUT_DIR ${DATADIR_BUILD}/gnucash/scm/${_OUTPUT_DIR_IN})
  ENDIF()
  FILE(MAKE_DIRECTORY ${_OUTPUT_DIR})
  # For guile 1, we simple link (or copy, for Windows) each source file to the dest directory
  IF(HAVE_GUILE1 OR MAKE_LINKS)
    SET(_SCHEME_LINKS "")
    FOREACH(scheme_file ${_SOURCE_FILES})
      SET(_SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${scheme_file})
      IF(IS_ABSOLUTE ${scheme_file})
        SET(_SOURCE_FILE ${scheme_file})
      ENDIF()
      GET_FILENAME_COMPONENT(name ${scheme_file} NAME)
      SET(_OUTPUT_FILE ${_OUTPUT_DIR}/${name})
      IF(NOT EXISTS ${_OUTPUT_FILE})
        LIST(APPEND _SCHEME_LINKS ${_OUTPUT_FILE})
        ADD_CUSTOM_COMMAND(
            OUTPUT ${_OUTPUT_FILE}
            COMMAND ${CMAKE_COMMAND} -E ${_CMD} ${_SOURCE_FILE} ${_OUTPUT_FILE}
        )
      ENDIF()
    ENDFOREACH(scheme_file)
    IF(HAVE_GUILE1)
      ADD_CUSTOM_TARGET(${_TARGET} ALL DEPENDS ${_SCHEME_LINKS})
    ELSE()
      ADD_CUSTOM_TARGET(${_TARGET}-links ALL DEPENDS ${_SCHEME_LINKS})
    ENDIF()
  ENDIF(HAVE_GUILE1 OR MAKE_LINKS)

  IF(HAVE_GUILE2)
    # Construct the guile source and compiled load paths
    SET(_GUILE_LOAD_PATH "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_CURRENT_BINARY_DIR};${CMAKE_BINARY_DIR}/src/scm")  # to pick up generated build-config.scm
    SET(_GUILE_LOAD_COMPILED_PATH "${CMAKE_CURRENT_BINARY_DIR}")
    FOREACH (load_item ${_GUILE_LOAD_DIRS})
       LIST(APPEND _GUILE_LOAD_PATH ${CMAKE_SOURCE_DIR}/${load_item})
       LIST(APPEND _GUILE_LOAD_COMPILED_PATH ${CMAKE_BINARY_DIR}/${load_item})
    ENDFOREACH(load_item)

    # Construct the LD_LIBRARY_PATH
    SET(_GUILE_LD_LIBRARY_PATH ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE})
    FOREACH(ld_item ${_GUILE_LIBRARY_DIRS})
        LIST(APPEND _GUILE_LD_LIBRARY_PATH ${CMAKE_BINARY_DIR}/${ld_item}/${CMAKE_BUILD_TYPE})
    ENDFOREACH(ld_item)

    # And then the module path
    SET(_GNC_MODULE_PATH "")
    FOREACH(module_item ${_GUILE_MODULES})
      LIST(APPEND _GNC_MODULE_PATH ${CMAKE_BINARY_DIR}/${module_item}/${CMAKE_BUILD_TYPE})
    ENDFOREACH(module_item)

    # In CMake lists are text strings where the items are separated by semicolons ("a;b;c;d" for example).
    # Replace the semis with colons to construct the path environment variables
    STRING(REPLACE ";" ":" _GUILE_LOAD_PATH "${_GUILE_LOAD_PATH}")
    STRING(REPLACE ";" ":" _GUILE_LOAD_COMPILED_PATH "${_GUILE_LOAD_COMPILED_PATH}")
    STRING(REPLACE ";" ":" _GUILE_MODULES "{_GUILE_MODULES}")
    STRING(REPLACE ";" ":" _GUILE_LD_LIBRARY_PATH "${_GUILE_LD_LIBRARY_PATH}")
    STRING(REPLACE ";" ":" _GNC_MODULE_PATH "${_GNC_MODULE_PATH}")

    IF(GNC_BUILD_AS_INSTALL)
      SET(_GUILE_CACHE_DIR ${LIBDIR_BUILD}/gnucash/scm/ccache/2.0)
      SET(_GUILE_LOAD_PATH "${CMAKE_CURRENT_SOURCE_DIR}:${DATADIR_BUILD}/gnucash/scm")
      SET(_GUILE_LOAD_COMPILED_PATH ${_GUILE_CACHE_DIR})
#      SET(_GNC_MODULE_PATH  "${LIBDIR_BUILD}/${CMAKE_BUILD_TYPE}:${LIBDIR_BUILD}/gnucash/${CMAKE_BUILD_TYPE}")
      SET(_GNC_MODULE_PATH  "")
      SET(_GUILE_LD_LIBRARY_PATH ${_GNC_MODULE_PATH})
    ENDIF()
    IF (__DEBUG)
      MESSAGE("  ")
      MESSAGE("   GUILE_LOAD_PATH: ${_GUILE_LOAD_PATH}")
      MESSAGE("   GUILE_LOAD_COMPILED_PATH: ${_GUILE_LOAD_COMPILED_PATH}")
      MESSAGE("   GUILE_LD_LIBRARY_PATH: ${_GUILE_LD_LIBRARY_PATH}")
      MESSAGE("   GNC_MODULE_PATH: ${_GNC_MODULE_PATH}")
    ENDIF(__DEBUG)
    SET(_TARGET_FILES "")

    FOREACH(source_file ${_SOURCE_FILES})
      SET(guile_depends ${_GUILE_DEPENDS_IN})
      GET_FILENAME_COMPONENT(basename ${source_file} NAME_WE)

      SET(output_file ${basename}.go)
      SET(_TMP_OUTPUT_DIR_IN ${_OUTPUT_DIR_IN})
      IF (_TMP_OUTPUT_DIR_IN)
        SET(output_file ${_OUTPUT_DIR_IN}/${basename}.go)
      ENDIF()
      IF(GNC_BUILD_AS_INSTALL)
        SET(output_file ${_GUILE_CACHE_DIR}/${output_file})
        LIST(APPEND _TARGET_FILES ${output_file})
      ELSE()
        LIST(APPEND _TARGET_FILES ${CMAKE_CURRENT_BINARY_DIR}/${output_file})
      ENDIF()

      SET(source_file_abs_path ${CMAKE_CURRENT_SOURCE_DIR}/${source_file})
      IF (IS_ABSOLUTE ${source_file})
        SET(source_file_abs_path ${source_file})
      ENDIF()
      IF (__DEBUG)
        MESSAGE("ADD_CUSTOM_COMMAND: output = ${output_file}")
      ENDIF()
      SET(CMAKE_COMMMAND_TMP "")
      IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
        SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
      ENDIF()
      ADD_CUSTOM_COMMAND(
        OUTPUT ${output_file}
        COMMAND ${CMAKE_COMMAND_TMP}
           GNC_UNINSTALLED=${_GNC_UNINSTALLED}
           GNC_BUILDDIR=${CMAKE_BINARY_DIR}
           #DYLD_FALLBACK_LIBRARY_PATH=${Boost_LIBRARY_DIRS} # this is hack for OS X
           LD_LIBRARY_PATH="${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${_GUILE_LD_LIBRARY_PATH}"
           DYLD_LIBRARY_PATH="${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${_GUILE_LD_LIBRARY_PATH}"
           GUILE_LOAD_PATH=${_GUILE_LOAD_PATH}
           GUILE_LOAD_COMPILED_PATH=${_GUILE_LOAD_COMPILED_PATH}
           #GNC_MODULE_PATH=${_GNC_MODULE_PATH}
           GNC_MODULE_PATH="${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${GNC_MODULE_PATH}"
           ${GUILE_EXECUTABLE} -e '\(@@ \(guild\) main\)' -s ${GUILD_EXECUTABLE} compile -o ${output_file} ${source_file_abs_path}
        DEPENDS ${guile_depends}
        MAIN_DEPENDENCY ${source_file_abs_path}
      )
    ENDFOREACH(source_file)
    IF (__DEBUG)
      MESSAGE("TARGET_FILES are ${_TARGET_FILES}")
    ENDIF(__DEBUG)
    ADD_CUSTOM_TARGET(${_TARGET} ALL DEPENDS ${_TARGET_FILES})
    INSTALL(FILES ${_TARGET_FILES} DESTINATION ${SCHEME_INSTALLED_CACHE_DIR}/${_OUTPUT_DIR_IN})

  ENDIF(HAVE_GUILE2)
  INSTALL(FILES ${_SOURCE_FILES} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/${_OUTPUT_DIR_IN})
ENDMACRO(GNC_ADD_SCHEME_TARGETS)
